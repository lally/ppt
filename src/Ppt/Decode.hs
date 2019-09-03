module Ppt.Decode where

import System.Directory
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Ppt.Frame.ParsedRep hiding (FValue)
import Ppt.Frame.Layout
import Numeric (showHex)
import Foreign.Ptr
import Foreign.Storable
import Data.Maybe
import GHC.Float
import Data.Vector.Storable ((!), (!?))
import Foreign.Storable
import qualified Data.List as L
import Data.Vector.Storable.ByteString
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

data FrameElementValue = FEValue { _primValue :: PrimitiveValue
                                 , _frameMember :: FrameMember
                                 , _layoutMember :: LayoutMember }
                       | FESeqno { _seqNo :: Word32 }
                         deriving (Eq, Show)

data FrameValue = FValue { _frame :: Frame
                         , _values :: [FrameElementValue] }
                  deriving (Eq, Show)

data DecodedRow = DRow { _rElements :: [FrameElement]
                       , _rSeqno :: Int
                       , _rValues :: [PrimitiveValue]
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
  let descElem (FEValue p _ l) = (lName l) ++ " " ++ show p
      descElem (FESeqno n) = "# " ++ show n
  in n ++ ": " ++ (L.intercalate ", " $ map descElem vals)

findMember :: String -> FrameLayout -> Maybe FrameMember
findMember name layout =
  if length mems > 0 then Just (head mems) else Nothing
  where (Frame _ elements) = flFrame layout
        memberNamed s (FMemberElem mem@(FMember _ name _)) | (s == name) = Just mem
                                                           | otherwise = Nothing
        memberNamed _ _ = Nothing
        mems = mapMaybe (memberNamed name) elements

type MaybeIO = MaybeT IO

frMemName :: LayoutMember -> String
frMemName fl = case (lKind fl) of
    (LKMember (FMember _ nm _) _) -> nm
    _ -> undefined

readMember :: [LayoutMember] -> FrameLayout -> (V.Vector Word8, TargetInfo, Int) -> MaybeIO ( [FrameElementValue])
readMember [] _ _ = do return []
readMember (lmem:lmems) layout v@(vec, tinfo, startOffset) =
  case (lKind lmem) of
    (LKSeqno FrontSeq) -> do
      primValue <- MaybeT $ V.unsafeWith vec $ \ptr -> do
        value <- peekElemOff (castPtr (plusPtr ptr startOffset) :: Ptr Word32) 0
        return $ Just value
      rest <- readMember lmems layout v
      MaybeT $ return $ (:) <$> (pure $ FESeqno primValue) <*> (pure rest)
    (LKMember elem _) -> do
      let lIxOf mem = startOffset + (lOffset mem)
          mrequire :: Show a => Bool -> a -> MaybeIO a
          mrequire True a = MaybeT $ return $ Just a
          mrequire False msg = MaybeT $ do putStrLn $ show msg
                                           return Nothing
      primValue <- MaybeT $ V.unsafeWith vec $ \ptr -> do
       let ptrAdded = plusPtr ptr (lIxOf lmem)
       case (fmType elem) of
         PDouble -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Double) 0
                       return $ Just $ PVRational value
         PFloat -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Float) 0
                      return $ Just $ PVRational $ float2Double value
         PInt -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Word32)  0
                    return $ Just $ PVIntegral $ fromIntegral value
         PTime (ETimeSpec _) -> do high <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
                                   low <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 1
                                   return $ Just $ PVTime (fromIntegral high) (fromIntegral low)
         PTime ETimeVal-> do high <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
                             return $ Just $ PVTime (fromIntegral high) 0
         -- TODO(lally): Add decode support for these counters.
         PCounter (Just n) -> do val <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
                                 return $ Just $ PVCounter val n (PIntelCounter "" "" "")
         PCounter Nothing -> return $ Just $ PVCounter 0 0 PCNone
         PByte -> do val <- peekElemOff (castPtr ptrAdded :: Ptr Word8) 0
                     return $ Just $ PVIntegral (fromIntegral val)
      let thisResult = FEValue <$> (pure primValue) <*> (findMember (frMemName lmem) layout) <*> pure lmem
      rest <- readMember lmems layout v
      --MaybeT $ do putStrLn $ "Got member: " ++ show primValue ++ " for found member " ++ (
      --              show $ findMember (frMemName lmem) layout) ++ " for layout mem " ++ show lmem
      --            return $ Just ()
      MaybeT $ return $ (:) <$> thisResult <*> (pure rest)
        -- TODO: implement the rest of these.  Look at what I can do in Vector to read different types.
        -- this probably needs to move to IO (Maybe [FrameElementValue]) and use Ptr and cast to read
        -- what I want.
    _ -> readMember lmems layout v

decodeFromBuffer :: TargetInfo -> V.Vector Word8 -> Int -> Int -> [FrameLayout] -> MaybeIO (FrameValue)
decodeFromBuffer tinfo vec startOffset sz layouts =
  do let genSpec = layoutSpec $ head layouts
         size = lioSize genSpec
         lIxOf mem = startOffset + (lOffset mem)
         memFrontSeq = head $ filter (\m -> lKind m == (LKSeqno FrontSeq)) $ flLayout $ head layouts
         memBackSeq = head $ filter (\m -> lKind m == (LKSeqno BackSeq)) $ flLayout $ head layouts
         memTypeDescs = filter (\m -> isTypeDesc $ lKind m) $ flLayout $ head layouts
                        where isTypeDesc (LKTypeDescrim _) = True
                              isTypeDesc _ = False
         nrFrameTypes =
           case memTypeDescs of
             [] -> 1
             (mt:_) -> let (LKTypeDescrim sz) = lKind mt in sz
         mrequire :: Show a => Bool -> a -> MaybeIO a
         mrequire True a = MaybeT $ return $ Just a
         mrequire False msg = MaybeT $ do putStrLn $ show msg
                                          return Nothing
         mpred True _ a = MaybeT $ a
         mpred False d _ = MaybeT $ return $ Just d
         readVecInt :: V.Vector Word8 -> Int -> MaybeIO Int
         readVecInt vec i = MaybeT $ V.unsafeWith vec (\ptr -> do let p = castPtr ptr :: Ptr Int
                                                                  val <- peekElemOff p i
                                                                  return (Just val))
         mlog s = MaybeT $ do putStrLn s
                              return (Just s)

     mrequire (sz == size) "Frame sizes are equal"
     mrequire (startOffset + sz <= V.length vec) ("Frame fits in vector", startOffset + sz, V.length vec)
     mrequire (lIxOf memFrontSeq == startOffset) "Frame seqno is first"
     startSeqno <- readVecInt vec startOffset
     endSeqno <- readVecInt vec (lIxOf memBackSeq)
     mrequire (startSeqno == endSeqno) ("Sequence numbers match", startSeqno, endSeqno)
     frameType <- if nrFrameTypes > 1
       then do -- mlog $ "Reading " ++ (show $ head memTypeDescs) ++ " at loffset " ++ show startOffset
               readVecInt vec (lIxOf $ head memTypeDescs)
       else MaybeT $ return $ Just 0
     mrequire (frameType < nrFrameTypes) ("Frame type is defined", frameType, nrFrameTypes)
     let layout = layouts !! frameType
     -- TODO: Go applicative on list constructor for members
     members <- readMember (flLayout layout) layout (vec, tinfo, startOffset)
     mlog $ "Got frame";
     return (FValue (flFrame layout) members)


-- TODO: Make this a lazy bytestring input, then use fromChunks to get
-- out strict ByteStrings, and combine the chunks (copying only the
-- bytes needed! as we cross boundaries.
decodeFromBytes :: TargetInfo -> JsonRep -> BSL.ByteString -> IO ([FrameValue])
decodeFromBytes tinfo json bytes = do
  case frameSize json of
    Nothing -> return []
    Just frsize -> do
      let frameLayouts = jsBufferFrames json
          vec :: V.Vector Word8
          vec = V.fromList $ BSL.unpack bytes
          bslen = BSL.length bytes
--      putStrLn $ "Frame size is " ++ show frsize ++ " bytes long,  Expecting " ++ show (
--        bslen `div` (fromIntegral frsize)) ++ " frames."
--      putStrLn $ "started with ByteString of length " ++ (show bslen)
      let numRecords = fromIntegral (BSL.length bytes `div` (fromIntegral frsize))
      res <- mapM (\idx -> runMaybeT $ decodeFromBuffer tinfo vec (idx * frsize) frsize frameLayouts) [0..numRecords]
      putStrLn $ " decoded " ++ show (length res) ++ " records:\n\t" ++ (L.intercalate "\n\t" $ map show res)
      return $ catMaybes res

deserialiseHeader :: DBG.Get (Maybe Word32)
deserialiseHeader = do
  prefix <- DBG.getWord32be
  if prefix /= 0x50505431
    then return Nothing
    else do length <- DBG.getWord32be
            return $ Just length

splitFileContents :: BSL.ByteString -> Maybe (FileRecord, BSL.ByteString)
splitFileContents contents =
  let length = DBG.runGet deserialiseHeader contents
  in case length of
    Nothing -> Nothing
    Just len ->
      let fileRecordBlob = BSL.take (fromIntegral len) $ BSL.drop 8 contents
          binaryFrames = BSL.drop ((fromIntegral len) + 8) contents
          mfileRecord = decode fileRecordBlob :: Maybe FileRecord
      in maybe Nothing (\v -> Just (v, binaryFrames)) mfileRecord

decodeFile :: BSL.ByteString -> IO ([FrameValue])
decodeFile contents = do
  let length = DBG.runGet deserialiseHeader contents
      header = splitFileContents contents
  case header of
    Nothing -> do putStrLn "Invalid file format"
                  return []
    Just (fileRecord, binaryFrames) -> do
      -- split up the string into three sections:
      -- (1) the 8 byte header we just read
      -- (2) the json blob
      -- (3) the binary frames
      putStrLn $ "Got remaining " ++ (show $ BSL.length binaryFrames) ++ " for file after header."
      putStrLn $ "That should be " ++ (show $ ((fromIntegral $ BSL.length binaryFrames) / 4.0)) ++ " Ints."
      decodeFromBytes (jsTarget $ frJson fileRecord) (frJson fileRecord) binaryFrames

-- We do a terrible job about supporting multiple platforms.  These
-- values don't indicate the actual sizes of some important stuff,
-- like time values or counters.
--x64 = TargetInfo 8 4 4 8 8 1

decodeFileToConsole :: String -> Int -> IO ()
decodeFileToConsole filename maxNr = do
  contents <- BSL.readFile filename
  values <- decodeFile contents
  putStrLn $ ">> " ++ (L.intercalate "\n>> " $ map descValue $ take maxNr values)

-- Always returns the sequence number *first*.  Also assumes its first.
decodeValue :: FrameValue -> (Int, [PrimitiveValue])
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
          layoutMems = mapMaybe (\c -> case c of (FEValue _ _ m) -> Just m ; (FESeqno _) -> Nothing) vals
      in HM.insertWith mergeEntry fr (makeEntry fr layoutMems seq row) mp

-- |Show a raw value given a time representation config.
showValue :: ETimeRep -> PrimitiveValue -> String
showValue (ETimeSpec _) (PVTime a b) = show $ a * 1000000000 + b
showValue (ETimeVal) (PVTime a b) = show $ a * 1000000 + b
showValue _ (PVRational d) = show d
showValue _ (PVIntegral i) = show i
showValue _ (PVCounter v 0 (PIntelCounter name _ _)) = name ++ ": " ++ show v
showValue _ (PVCounter v 1 (PIntelCounter _ name _)) = name ++ ": " ++ show v
showValue _ (PVCounter v 2 (PIntelCounter _ _ name)) = name ++ ": " ++ show v
showValue _ (PVCounter _ _ PCNone) = "0"


-- |Currently does no processing. Opens 'filename' and writes out CSVs
-- - one per found frame type - to 'destDir'.
decodeFileToCSVs filename destDir = do
  let writeCsv dir (DFrame fr lmem rows) = do
        let fileName = (_frameName fr) ++ ".csv"
            firstRow = head rows
            elemName (FMemberElem (FMember _ n _)) = n
            elemName (FCalculatedElem _ _ n _ _) = n
            timeRep = let etr (PTime t) = Just t
                          etr _ = Nothing
                          elems = mapMaybe (etr . lType) lmem
                      in if length elems > 0 then head elems else ETimeVal
            headers = "ppt_seqno, " ++( L.intercalate ", " $ map lName lmem)
            saveRow (DRow _ n vs) = L.intercalate ", " $ (show n):(map (showValue timeRep) vs)
        h <- openFile (destDir ++ "/" ++ fileName) WriteMode
        hPutStrLn h headers
        mapM_ (\r -> hPutStrLn h $ saveRow r) rows
        hClose h
  -- This will fail quickly if we can't create the directory.
  createDirectory destDir
  contents <- BSL.readFile filename
  values <- decodeFile {-x64-} contents
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
