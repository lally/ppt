module Ppt.Decode where

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
    (LKMember frel _) ->
      case frel of
        (FMemberElem (FMember _ nm _)) -> nm
        (FCalculatedElem _ _ nm _ _) -> nm
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
    (LKMember (FMemberElem elem) _) -> do
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
         -- TODO: support both time formats
         PTime -> do high <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 0
                     low <- peekElemOff (castPtr ptrAdded :: Ptr Word64) 1
                     return $ Just $ PVTime (fromIntegral high) (fromIntegral low)
         PCounter -> return $ Just $ PVCounter []
         PByte -> return Nothing -- $ Just $ PVIntegral 0
      let thisResult = FEValue <$> (pure primValue) <*> (findMember (frMemName lmem) layout) <*> pure lmem
      rest <- readMember lmems layout v
--      MaybeT $ do putStrLn $ "Got member: " ++ show primValue ++ " for found member " ++ (
--                    show $ findMember (frMemName lmem) layout) ++ " for layout mem " ++ show lmem
--                  return $ Just ()
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
                                                                  val <- peek p
                                                                  return (Just val))

     mrequire (sz == size) "Frame sizes are equal"
     mrequire (startOffset + sz <= V.length vec) ("Frame fits in vector", startOffset, V.length vec)
     mrequire (lIxOf memFrontSeq == startOffset) "Frame seqno is first"
     startSeqno <- readVecInt vec startOffset
     endSeqno <- readVecInt vec (lIxOf memBackSeq)
     mrequire (startSeqno == endSeqno) ("Sequence numbers match", startSeqno, endSeqno)
     frameType <- if nrFrameTypes > 1
       then readVecInt vec (lIxOf $ head memTypeDescs)
       else MaybeT $ return $ Just 0
     mrequire (frameType < nrFrameTypes) ("Frame type is defined", frameType, nrFrameTypes)
     let layout = layouts !! frameType
     -- TODO: Go applicative on list constructor for members
     members <- readMember (flLayout layout) layout (vec, tinfo, startOffset)
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
--      putStrLn $ " decoded " ++ show (length res) ++ " records:\n\t" ++ (L.intercalate "\n\t" $ map show res)
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


decodeFile :: TargetInfo -> BSL.ByteString -> IO ([FrameValue])
decodeFile tinfo contents = do
  let -- lazy = BSL.fromChunks [contents]
      length = DBG.runGet deserialiseHeader contents
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
      decodeFromBytes tinfo (frJson fileRecord) binaryFrames

-- We do a terrible job about supporting multiple platforms.  These
-- values don't indicate the actual sizes of some important stuff,
-- like time values or counters.
x64 = TargetInfo 8 4 4 8 8 1

decodeFileToConsole :: String -> Int -> IO ()
decodeFileToConsole filename maxNr = do
  contents <- BSL.readFile filename
  values <- decodeFile x64 contents
  putStrLn $ ">> " ++ (L.intercalate "\n>> " $ map descValue $ take maxNr values)
