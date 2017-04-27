module Ppt.Decode where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Ppt.Frame.ParsedRep hiding (FValue)
import Ppt.Frame.Layout
import Data.Maybe
import Data.Vector.Storable ((!), (!?))
import Foreign.Storable
import Data.Vector.Storable.ByteString
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Data.Aeson (decode) 

data FrameElementValue = FEValue { _primValue :: PrimitiveValue
                                 , _frameMember :: FrameMember
                                 , _layoutMember :: LayoutMember }
                         deriving (Eq, Show)

data FrameValue = FValue { _frame :: Frame
                         , _values :: [FrameElementValue] }
                  deriving (Eq, Show)

findMember :: String -> FrameLayout -> Maybe FrameMember
findMember name layout = mrequire (length mems == 1) $ head mems
                         where (Frame _ elements) = flFrame layout
                               memberNamed s (FMemberElem mem@(FMember _ name _)) | (s == name) = Just mem
                                                                                  | otherwise = Nothing
                               memberNamed _ _ = Nothing
                               mems = mapMaybe (memberNamed name) elements
                               mrequire :: Bool -> a -> Maybe a
                               mrequire True a = Just a
                               mrequire False _ = Nothing

type MaybeIO = MaybeT IO

readMember :: [LayoutMember] -> FrameLayout -> (V.Vector Int, TargetInfo, Int) -> MaybeIO ( [FrameElementValue])
readMember [] _ _ = do return []
readMember (lmem:lmems) layout v@(vec, tinfo, startIdx) =
  case (lKind lmem) of
    (LKMember (FMemberElem elem) _) -> do
      let ixOf off = off `div` (tInt tinfo)
          lIxOf mem = startIdx + (ixOf $ lOffset mem)
      primValue <- MaybeT $ V.unsafeWith vec $ \ptr -> case (fmType elem) of
         PDouble -> return Nothing
         PFloat -> return Nothing
         PInt -> do value <- peekElemOff ptr (lIxOf lmem)
                    return $ Just $ PVIntegral value
         PTime -> do high <- peekElemOff ptr (lIxOf lmem)
                     low <- peekElemOff ptr (1 + lIxOf lmem)
                     return $ Just $ PVTime high low
         PCounter -> return Nothing
         PByte -> return Nothing
      let thisResult = FEValue <$> (pure primValue) <*> (findMember (lName lmem) layout) <*> pure lmem
      rest <- readMember lmems layout v
      MaybeT $ return $ (:) <$> thisResult <*> (pure rest)
        -- TODO: implement the rest of these.  Look at what I can do in Vector to read different types.
        -- this probably needs to move to IO (Maybe [FrameElementValue]) and use Ptr and cast to read
        -- what I want.
    _ -> readMember lmems layout v

decodeFromBuffer :: TargetInfo -> V.Vector Int -> Int -> Int -> [FrameLayout] -> MaybeIO (FrameValue)
decodeFromBuffer tinfo vec startIdx sz layouts =
  do let genSpec = layoutSpec $ head layouts
         size = lioSize genSpec
         ixOf off = off `div` (tInt tinfo)
         lIxOf mem = startIdx + (ixOf $ lOffset mem)
         memFrontSeq = head $ filter (\m -> lKind m == (LKSeqno FrontSeq)) $ flLayout $ head layouts
         memBackSeq = head $ filter (\m -> lKind m == (LKSeqno BackSeq)) $ flLayout $ head layouts
         memTypeDesc = head $ filter (\m -> isTypeDesc $ lKind m) $ flLayout $ head layouts
                       where isTypeDesc (LKTypeDescrim _) = True
                             isTypeDesc _ = False
         nrFrameTypes = let (LKTypeDescrim sz) = lKind memTypeDesc in sz
         mrequire :: Bool -> a -> MaybeIO a
         mrequire True a = MaybeT $ return $ Just a
         mrequire False _ = MaybeT $ return $ Nothing
         readVecInt :: V.Vector Int -> Int -> MaybeIO Int
         readVecInt vec i = MaybeT $ return $ vec !? i
     mrequire (sz == size) "Frame sizes are equal"
     mrequire (sz + startIdx <= V.maxIndex vec) "Frame fits in vector"
     mrequire (lIxOf memFrontSeq == startIdx) "Frame seqno is first"
     startSeqno <- readVecInt vec startIdx
     endSeqno <- readVecInt vec (lIxOf memBackSeq)
     mrequire (startSeqno == endSeqno) "Sequence numbers match"
     frameType <- readVecInt vec (lIxOf memTypeDesc)
     mrequire (frameType < nrFrameTypes) "Frame type is defined"
     let layout = layouts !! frameType
     -- Go applicative on list constructor for members
     members <- readMember (flLayout layout) layout (vec, tinfo, startIdx)
     return (FValue (flFrame layout) members)


-- TODO: Make this a lazy bytestring input, then use fromChunks to get
-- out strict ByteStrings, and combine the chunks (copying only the
-- bytes needed! as we cross boundaries.
decodeFromBytes :: TargetInfo -> JsonRep -> BS.ByteString -> IO ([FrameValue])
decodeFromBytes tinfo json bytes = do
  let frameLayouts = jsBufferFrames json
      vec :: V.Vector Int
      vec = byteStringToVector bytes
  case frameSize json of
    Nothing -> return []
    Just frsize -> do
      res <- mapM (\idx -> runMaybeT $ decodeFromBuffer tinfo vec (idx * frsize) frsize frameLayouts) [0..]
      return $ catMaybes res

deserialiseHeader :: Get (Maybe Word32)
deserialiseHeader = do
  prefix <- getWord32be
  if prefix /= 0x50505431
    then return Nothing
    else do length <- getWord32be
            return $ Just length

decodeFile :: TargetInfo -> BS.ByteString -> IO ([FrameValue])
decodeFile tinfo contents = do
  let lazy = BSL.fromChunks [contents]
      length = runGet deserialiseHeader lazy
  case length of
    Nothing -> do putStrLn "Invalid file format"
                  return []
    Just len_ -> do
      -- split up the string into three sections:
      -- (1) the 8 byte header we just read
      -- (2) the json blob
      -- (3) the binary frames
      let len = fromIntegral len_
          jsonBlob = BSL.take len $ BSL.drop 8 lazy
          binaryFrames = BS.drop ((fromIntegral len_) + 8) contents
          mJson = decode jsonBlob
      case mJson of
        Nothing -> do putStrLn "Couldn't decode file metadata"
                      return []
        Just json -> decodeFromBytes tinfo json binaryFrames
