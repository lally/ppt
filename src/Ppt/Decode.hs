{-# LANGUAGE LambdaCase #-} 
module Ppt.Decode where

import System.Directory
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Lens
import Ppt.Frame.ParsedRep hiding (FValue)
import Ppt.Frame.Types
import Ppt.Frame.Layout
import Numeric (showHex)
import Foreign.Ptr
import Foreign.Storable
import Data.Int (Int32)
import Data.Maybe
import GHC.Float
import Data.Vector.Storable ((!), (!?))
import Foreign.Storable
import qualified Data.List as L
import Data.Vector.Storable.ByteString
import Safe
import System.IO
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as B
import qualified Data.Binary as DB
import qualified Data.Binary.Get as DBG
import Data.Word
import Data.Aeson (decode)

data FrameElementValue = FEValue { _primValue :: Prim
                                 , _frameMember :: FrameMember
                                 , _layoutMember :: LayoutMember }
                       | FESeqno { _seqNo :: Word32 }
                         deriving (Eq, Show)

data FrameValue = FValue { _frame :: Frame
                         , _values :: [FrameElementValue] }
                  deriving (Eq, Show)

data DecodedRow = DRow { _rElements :: [FrameElement]
                       , _rSeqno :: Int
                       , _rValues :: [Prim]
                       } deriving (Eq, Show)

data DecodedFrame = DFrame { _dFrame :: Frame
                           , _dLayout :: [LayoutMember]
                           , _dRows :: [DecodedRow]
                           } deriving (Eq, Show)

hexList [] = ""
hexList xs = (concatMap hex thisWord) ++ (' ':(hexList rest))
             where hex n = if n < 16 then "0" ++ (showHex n " ") else showHex n " "
                   (thisWord, rest) = splitAt 4 xs

descValue :: FrameValue -> String
descValue (FValue (Frame n _) vals) =
  let descElem (FEValue p _ l) = (l ^. lName) ++ " " ++ show p
      descElem (FESeqno n) = "# " ++ show n
  in n ++ ": " ++ (L.intercalate ", " $ map descElem vals)

findMember :: String -> FrameLayout -> Maybe FrameMember
findMember name layout =
  if length mems > 0 then Just (head mems) else Nothing
  where (Frame _ elements) = layout ^. flFrame
        memberNamed s (FMemberElem mem@(FMember _ name _))
          | (s == name) = Just mem
          | otherwise = Nothing
        memberNamed _ _ = Nothing
        mems = mapMaybe (memberNamed name) elements

type MaybeIO = MaybeT IO

frMemName :: LayoutMember -> String
frMemName fl = case (fl ^. lKind) of
    (LKMember (FMember _ nm _) _) -> nm
    _ -> undefined

readMember :: [LayoutMember] -> ReadConfig -> FrameLayout -> (V.Vector Word8, ReadConfig, Int) -> MaybeIO ( [FrameElementValue])
readMember [] _ _ _ = do return []
readMember (lmem:lmems) rconf layout v@(vec, rinfo, startOffset) =
  case lmem ^. lKind of
    LKSeqno FrontSeq -> do
      primValue <- MaybeT $ V.unsafeWith vec $ \ptr -> do
        value <- peekElemOff (castPtr (plusPtr ptr startOffset) :: Ptr Word32) 0
        return $ Just value
      rest <- readMember lmems rconf layout v
      MaybeT $ return $ (:) <$> (pure $ FESeqno primValue) <*> (pure rest)
    LKMember elem _ -> do
      let lIxOf mem = startOffset + (mem ^. lOffset)
          mrequire :: Show a => Bool -> a -> MaybeIO a
          mrequire True a = MaybeT $ return $ Just a
          mrequire False msg = MaybeT $ do putStrLn $ show msg
                                           return Nothing
      primValue <- MaybeT $ liftM Just $ readValue (lmem ^. lType) rconf vec (lIxOf lmem)
      let thisResult = FEValue <$> (pure primValue) <*> (findMember (frMemName lmem) layout) <*> pure lmem
      rest <- readMember lmems rconf layout v
      --MaybeT $ do putStrLn $ "Got member: " ++ show primValue ++ " for found member " ++ (
      --              show $ findMember (frMemName lmem) layout) ++ " for layout mem " ++ show lmem
      --            return $ Just ()
      MaybeT $ return $ (:) <$> thisResult <*> (pure rest)
    _ -> readMember lmems rconf layout v   -- Either back seqno (already checked), padding, or type desc.

decodeFromBuffer :: ReadConfig -> V.Vector Word8 -> Int -> Int -> [FrameLayout] -> MaybeIO (FrameValue)
decodeFromBuffer rinfo vec startOffset sz layouts =
  do let genSpec = layoutSpec $ head layouts
         size = genSpec ^.lioSize
         lIxOf mem = startOffset + (mem ^. lOffset)
         firstMembers = (head layouts) ^. flLayout

         memFrontSeq = head $ firstMembers ^.. folded.filtered (\m -> m ^. lKind == (LKSeqno FrontSeq))
         memBackSeq = head $ firstMembers ^.. folded.filtered (\m -> m ^. lKind == (LKSeqno BackSeq))

--         memTypeDescs :: [LayoutMember]
         memTypeDescs = firstMembers ^.. folded.filtered (\m -> has _LKTypeDescrim (m ^. lKind))
         nrFrameTypes = headDef 1 (memTypeDescs ^.. folded.lKind._LKTypeDescrim)

         showBytes :: IO ()
         showBytes = do frameBytes <- V.unsafeWith vec (\ptr -> forM [0..sz-1] (\i -> peekByteOff ptr (startOffset + i))) :: IO [Word8]
                        putStrLn $ show (hexList frameBytes)
         mrequire :: Show a => Bool -> a -> MaybeIO a
         mrequire True a = MaybeT $ return $ Just a
         mrequire False msg = MaybeT $ do putStrLn $ show msg
                                          showBytes
                                          return Nothing
         mpred True _ a = MaybeT $ a
         mpred False d _ = MaybeT $ return $ Just d
         readVecInt :: V.Vector Word8 -> Int -> MaybeIO Int32
         readVecInt vec i = MaybeT $ V.unsafeWith vec (\ptr -> do let p = castPtr ptr :: Ptr Int32
                                                                  val <- peekElemOff p (i `div` 4)
                                                                  return (Just val))
         mlog s = MaybeT $ do putStrLn s
                              return (Just s)

     mrequire (sz == size) "Frame sizes are equal"
     mrequire (startOffset + sz <= V.length vec) ("Frame fits in vector (start offset + size, length of vec)",
                                                  startOffset + sz, V.length vec)
     mrequire (lIxOf memFrontSeq == startOffset) "Frame seqno is first"
     mrequire (size - tInt (rcTargetInfo rinfo) == memBackSeq ^. lOffset) "Back seqno is last"

     startSeqno <- readVecInt vec (lIxOf memFrontSeq)
     endSeqno <- readVecInt vec (lIxOf memBackSeq)

     if startSeqno /= endSeqno
       then MaybeT $ do putStrLn ( "* Failed synchronization.  Starting offset is " ++ show startOffset ++ " and bytes are:")
                        showBytes
                        putStrLn $ "Got start = " ++ show startSeqno ++ ", and end = " ++ show endSeqno
                        return Nothing
       else MaybeT $ return (Just 0)
     mrequire (startSeqno == endSeqno) ("Sequence numbers match", startSeqno, endSeqno)

     frameType <- if nrFrameTypes > 1
       then do mlog $ "Reading " ++ show (head memTypeDescs) ++ " at loffset " ++ show startOffset
               MaybeT $ do showBytes
                           return (Just 1)
               rawFrameType <- readVecInt vec (lIxOf $ head memTypeDescs)
               MaybeT $ return $ Just (fromIntegral rawFrameType :: Int)
       else MaybeT $ return $ Just 0

     mrequire (frameType < nrFrameTypes) ("Frame type discriminator in range (frame type, nr values)", frameType, nrFrameTypes)

     let layout = layouts !! frameType
     members <- readMember (layout ^. flLayout) rinfo layout (vec, rinfo, startOffset)
     return (FValue (layout ^. flFrame) members)

-- |Applies collection parameters (time stamp type, performance
-- counter names) to the member definitions.
configureLayouts :: FileRecord -> [FrameLayout]
configureLayouts frecord =
  let json = frJson frecord
      counterConfig = case frCounters frecord of
        [] -> Just (PPCNone, 0)
        xs -> Just (PPIntelCounter (x !! 0) (x !! 1) (x !! 2), 0)
              where x = xs ++ repeat ""
      timeConfig = Just ((jsBufferEmit (frJson frecord)) ^. eTimeRep, 0, 0)
      convertMember :: LayoutMember -> LayoutMember
      convertMember mem =
        case mem ^. lKind of
          LKMember fm@(FMember (PTime t) _ _) side ->
            mem { _lKind = LKMember (fm { fmType = PTime timeConfig }) side }
          LKMember fm@(FMember (PCounter e v) _ _) side ->
            mem { _lKind = LKMember (fm { fmType = PCounter e counterConfig }) side }
          _ -> mem
      convertLayout :: FrameLayout -> FrameLayout
      convertLayout layout =
        layout { _flLayout = map convertMember (layout ^. flLayout) }
  in map convertLayout (jsBufferFrames json)

-- TODO: Make this a lazy bytestring input, then use fromChunks to get
-- out strict ByteStrings, and combine the chunks (copying only the
-- bytes needed! as we cross boundaries.
decodeFromBytes :: FileRecord  -> BSL.ByteString -> IO [FrameValue]
decodeFromBytes frecord bytes = do
  let rinfo = readConfig frecord
      json = frJson frecord
  case frameSize json of
    Nothing -> return []
    Just frsize -> do
      let frameLayouts = configureLayouts frecord
          vec :: V.Vector Word8
          vec = V.fromList $ BSL.unpack bytes
      let numRecords = fromIntegral (BSL.length bytes `div` fromIntegral frsize)
      res <- mapM (\idx -> runMaybeT $ decodeFromBuffer rinfo vec (idx * frsize) frsize frameLayouts) [0..numRecords]
      let successful = catMaybes res
      putStrLn $ " decoded " ++ show (length successful) ++ " records successfully out of "++ show (length res) ++ ":\n\t" ++ L.intercalate "\n\t" (map show res)
      return $ catMaybes res

showBytesForFrame :: FileRecord -> BSL.ByteString -> Int -> IO ()
showBytesForFrame frecord bytes nr = do
  let json = frJson frecord
  case frameSize json of
    Nothing -> putStrLn "Framesize is Nothing"
    Just frsize -> do
      let rawBytes :: [Word8]
          rawBytes = BSL.unpack $ BSL.take (fromIntegral frsize) bytes
      putStrLn ("Bytes for frame " ++ show nr ++ ": " ++ hexList rawBytes)


decodeOneFromBytes :: FileRecord -> BSL.ByteString -> Int -> IO (Maybe FrameValue)
decodeOneFromBytes frecord bytes nr = do
  let rinfo = readConfig frecord
      json = frJson frecord
  case frameSize json of
    Nothing -> do putStrLn "Framesize is Nothing"
                  return Nothing
    Just frsize -> do
      let frameLayouts = jsBufferFrames json
          rawBytes :: [Word8]
          rawBytes = BSL.unpack $ BSL.take (fromIntegral frsize) bytes
          vec = V.fromList rawBytes
      putStrLn ("Reading " ++ hexList rawBytes)
      runMaybeT $ decodeFromBuffer rinfo vec (nr * frsize) frsize frameLayouts


deserialiseHeader :: DBG.Get (Maybe Word32)
deserialiseHeader = do
  prefix <- DBG.getWord32be
  if prefix /= 0x50505431
    then return Nothing
    else Just <$> DBG.getWord32be

-- split up the string into three sections:
-- (1) the 8 byte header we just read
-- (2) the json blob
-- (3) the binary frames
splitFileContents :: BSL.ByteString -> Maybe (FileRecord, BSL.ByteString, Int)
splitFileContents contents =
  let length = DBG.runGet deserialiseHeader contents
  in case length of
    Nothing -> Nothing
    Just len ->
      let fileRecordBlob = BSL.take (fromIntegral len) $ BSL.drop 8 contents
          binaryFrames = BSL.drop (fromIntegral len + 8) contents
          mfileRecord = decode fileRecordBlob :: Maybe FileRecord
      in (\v -> Just (v, binaryFrames, 8 + fromIntegral len)) =<< mfileRecord

readConfig :: FileRecord -> ReadConfig
readConfig fr =
  let counterNames = frCounters fr
      infCounterNames = counterNames ++ repeat ""
      counters = if null counterNames
                 then PPCNone
                 else PPIntelCounter (infCounterNames !! 0) (infCounterNames !! 1) (infCounterNames !! 2)
      json = frJson fr
  in ReadConfig (jsTarget json) (jsBufferEmit json ^. eTimeRep) counters

decodeFile :: BSL.ByteString -> IO ([FrameValue])
decodeFile contents = do
  let header = splitFileContents contents
  case header of
    Nothing -> do putStrLn "Invalid file format"
                  return []
    Just (fileRecord, binaryFrames, headerLen) -> do
      let length = DBG.runGet deserialiseHeader contents
          json = frJson fileRecord
          layoutSpecs = map (\f -> let (LayoutIO ss _) = layoutSpec f in ss) (jsBufferFrames json)
      putStrLn $ "Frame record sizes are (and should all be equal): " ++ L.intercalate ", " (map show layoutSpecs)
      putStrLn $ "Got remaining " ++ show (BSL.length binaryFrames) ++ " for file after header."
      putStrLn $ "That should be " ++ show (fromIntegral (BSL.length binaryFrames) / fromIntegral (head layoutSpecs)) ++ " Frames."
      putStrLn $ "File header was " ++ show headerLen ++ " bytes."
      decodeFromBytes fileRecord binaryFrames

decodeFileToConsole :: String -> Int -> IO ()
decodeFileToConsole filename maxNr = do
  contents <- BSL.readFile filename
  values <- decodeFile contents
  putStrLn $ ">> " ++ (L.intercalate "\n>> " $ map descValue (take maxNr values))

-- Always returns the sequence number *first*.  Also assumes its first.
decodeValue :: FrameValue -> (Int, [Prim])
decodeValue (FValue fr ((FESeqno s):vals)) =
  (fromIntegral s, map getPrim vals)
  where getPrim (FEValue p _ _) = p

-- |Prototypical simple sorter.  Sorts decodded FrameValues into a
-- list of DecocdedFrame values.  Each element in the output list
-- contains a list of rows.  Each input corresponds to a row.
sortValues :: [FrameValue] -> [DecodedFrame]
sortValues inputs =
  map snd $ HM.toList $ foldr updateForFV HM.empty inputs
  where
    mergeEntry :: DecodedFrame -> DecodedFrame -> DecodedFrame
    mergeEntry dfr (DFrame _ _ []) = dfr
    mergeEntry (DFrame fr l rows) (DFrame _ _ r) = DFrame fr l (rows ++ r)
    makeEntry fr lmem seq row = DFrame fr lmem [DRow (_frameElements fr) seq row]
    updateForFV :: FrameValue -> HM.HashMap Frame DecodedFrame -> HM.HashMap Frame DecodedFrame
    updateForFV fv@(FValue fr vals) mp =
      let (seq, row) = decodeValue fv
          layoutMems = mapMaybe (\case (FEValue _ _ m) -> Just m ; (FESeqno _) -> Nothing) vals
      in HM.insertWith mergeEntry fr (makeEntry fr layoutMems seq row) mp

-- |Show a raw value given a time representation config.
showValue :: Prim -> String
showValue (PTime (Just (ETimeSpec _, a, b))) = show $ a * 1000000000 + b
showValue (PTime (Just (ETimeVal, a, b))) = show $ a * 1000000 + b
-- these two should probably truncate
showValue (PRational _ (Just d)) = show d
showValue (PIntegral _ (Just i)) = show i
showValue (PCounter (Expanded _) (Just (_,  v))) = show v

-- |Currently does no processing. Opens 'filename' and writes out CSVs
-- - one per found frame type - to 'destDir'.
decodeFileToCSVs filename destDir = do
  let writeCsv dir (DFrame fr lmem rows) = do
        let fileName = _frameName fr ++ ".csv"
            firstRow = head rows
            nameOf lm =
              case memLayoutType lm  of
                Just (PCounter (Expanded 0) (Just (PPIntelCounter a _ _, v))) -> (lm ^. lName) ++ ": " ++ a
                Just (PCounter (Expanded 1) (Just (PPIntelCounter _ b _, v))) -> (lm ^. lName) ++ ": " ++ b
                Just (PCounter (Expanded 2) (Just (PPIntelCounter _ _ c, v))) -> (lm ^. lName) ++ ": " ++ c
                _ -> lm ^. lName
            headers = "ppt_seqno, " ++  L.intercalate ", " (map nameOf $ lmem ^.. folded)
            saveRow (DRow _ n vs) = L.intercalate ", " $ show n:map showValue vs
        h <- openFile (destDir ++ "/" ++ fileName) WriteMode
        hPutStrLn h headers
        mapM_ (hPutStrLn h . saveRow) rows
        hClose h
  -- This will fail quickly if we can't create the directory.
  createDirectory destDir
  contents <- BSL.readFile filename
  values <- decodeFile contents
--  let csvs = F.foldl' (\hm fv -> HM.insertWith (++) (_frameName $ _frame fv) [fv] hm) HM.empty values
  let csvs = sortValues values
  mapM_ (writeCsv destDir) csvs
  return $ map (_frameName . _dFrame) csvs

decodeCommand :: [String] -> IO ()
decodeCommand [] = do
  putStrLn "usage: ppt decode input_filename [output_dir]"
  putStrLn "  where output_dir will be generated if unspecified."

decodeCommand (first:rest) = do
  let (console, filename) = if (head first) == '-'
                            then (True, head rest)
                            else (False, first)
  if console
    then decodeFileToConsole filename 50
    else do let outputDir = if length rest < 1
                            then filename ++ "_output"
                            else head rest
            result <- decodeFileToCSVs filename outputDir
            putStrLn $ "Decoded " ++ (L.intercalate ", " result ) ++ " into " ++ outputDir
  return ()
