module Ppt.Decode where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Ppt.Frame.ParsedRep hiding (FValue)
import Ppt.Frame.Layout
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
                         deriving (Eq, Show)

data FrameValue = FValue { _frame :: Frame
                         , _values :: [FrameElementValue] }
                  deriving (Eq, Show)

descValue :: FrameValue -> String
descValue (FValue (Frame n _) vals) =
  n ++ ": " ++ (L.intercalate ", " $ map (\(FEValue p _ l) -> (lName l) ++ " " ++ show p) vals)

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

readMember :: [LayoutMember] -> FrameLayout -> (V.Vector Int, TargetInfo, Int) -> MaybeIO ( [FrameElementValue])
readMember [] _ _ = do return []
readMember (lmem:lmems) layout v@(vec, tinfo, startIdx) =
  case (lKind lmem) of
    (LKMember (FMemberElem elem) _) -> do
      let ixOf off = off `div` (tInt tinfo)
          lIxOf mem = startIdx + (ixOf $ lOffset mem)
          mrequire :: Show a => Bool -> a -> MaybeIO a
          mrequire True a = MaybeT $ return $ Just a
          mrequire False msg = MaybeT $ do putStrLn $ show msg
                                           return Nothing
--      MaybeT $ do putStrLn $ "Loading member " ++ (
--                    show $ findMember (frMemName lmem) layout) ++ " for layout mem " ++ show lmem
--                  return $ Just ()
      primValue <- MaybeT $ V.unsafeWith vec $ \ptr -> do
       let ptrAdded = plusPtr ptr (4 * (lIxOf lmem))
--       putStrLn ("Got ptr " ++ show ptr ++ " with lIXOf mem=" ++ show (lIxOf lmem) ++ " with first elem " ++ show ptrAdded)
       case (fmType elem) of
         PDouble -> do value <- peekElemOff (castPtr ptrAdded) 0
                       return $ Just $ PVRational value
         PFloat -> do value <- peekElemOff (castPtr ptrAdded :: Ptr Float) 0
                      return $ Just $ PVRational $ float2Double value
         PInt -> do value <- peekElemOff ptrAdded 0
                    return $ Just $ PVIntegral value
         PTime -> do high <- peekElemOff ptrAdded 0
                     low <- peekElemOff ptrAdded 8
                     return $ Just $ PVTime high low
         PCounter -> return $ Just $ PVCounter []
         PByte -> return Nothing -- $ Just $ PVIntegral 0
      let thisResult = FEValue <$> (pure primValue) <*> (findMember (frMemName lmem) layout) <*> pure lmem
      rest <- readMember lmems layout v
      MaybeT $ do putStrLn $ "Got member: " ++ show primValue ++ " for found member " ++ (
                    show $ findMember (frMemName lmem) layout) ++ " for layout mem " ++ show lmem
                  return $ Just ()
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
         readVecInt :: V.Vector Int -> Int -> MaybeIO Int
         readVecInt vec i = MaybeT $ return $ vec !? i
     MaybeT $ do putStrLn ( "decodeFromBuffer : TargetInfo (vec of length " ++ (show $ V.maxIndex vec) ++ (
                              " ints) start index: " ++ (show startIdx) ++ " ints and " ++ (
                                  show $ length layouts) ++ " layouts"))
                 return $ Just "foof"
     mrequire (sz == size) "Frame sizes are equal"
     mrequire (startIdx <= V.maxIndex vec) ("Frame fits in vector", startIdx, V.maxIndex vec)
     mrequire (lIxOf memFrontSeq == startIdx) "Frame seqno is first"
     startSeqno <- readVecInt vec startIdx
     endSeqno <- readVecInt vec (lIxOf memBackSeq)
     mrequire (startSeqno == endSeqno) ("Sequence numbers match", startSeqno, endSeqno)
     frameType <- if nrFrameTypes > 1
       then readVecInt vec (lIxOf $ head memTypeDescs)
       else MaybeT $ return $ Just 0
     mrequire (frameType < nrFrameTypes) ("Frame type is defined", frameType, nrFrameTypes)
     let layout = layouts !! frameType
     -- Go applicative on list constructor for members
     members <- readMember (flLayout layout) layout (vec, tinfo, startIdx)
     return (FValue (flFrame layout) members)

getInts :: DBG.Get [Int]
getInts = do
  empty <- DBG.isEmpty
  if empty
    then return []
    else do value <- DBG.getWord32le
            rest <- getInts
            return (fromIntegral value:rest)

getVector :: DBG.Get (V.Vector Int)
getVector = do valList <- DB.get
               return $ V.fromList valList

-- TODO: Make this a lazy bytestring input, then use fromChunks to get
-- out strict ByteStrings, and combine the chunks (copying only the
-- bytes needed! as we cross boundaries.
decodeFromBytes :: TargetInfo -> JsonRep -> BSL.ByteString -> IO ([FrameValue])
decodeFromBytes tinfo json bytes = do
  case frameSize json of
    Nothing -> return []
    Just frsize -> do
      let frameLayouts = jsBufferFrames json
          ints = DBG.runGet getInts bytes
          vec :: V.Vector Int
          vec = V.fromList $ ints
          bslen = BSL.length bytes
      putStrLn $ "Frame size is " ++ show frsize ++ " bytes long," --  Expecting " ++ (bslen `div` (fromIntegral frsize)) ++ " frames."
      putStrLn $ "broke file into list of ints: " ++ show ints
      putStrLn $ "converted ByteString of length " ++ (show bslen) ++ " into int vector of length " ++ (show $ 1 +  V.maxIndex vec)
      let numRecords = fromIntegral (BSL.length bytes `div` (fromIntegral frsize))
      res <- mapM (\idx -> runMaybeT $ decodeFromBuffer tinfo vec ((idx * frsize) `div` 4) frsize frameLayouts) [0..numRecords]
      putStrLn $ " decoded " ++ show (length res) ++ " records:\n\t" ++ (L.intercalate "\n\t" $ map show res)
      return $ catMaybes res

deserialiseHeader :: DBG.Get (Maybe Word32)
deserialiseHeader = do
  prefix <- DBG.getWord32be
  if prefix /= 0x50505431
    then return Nothing
    else do length <- DBG.getWord32be
            return $ Just length

decodeFile :: TargetInfo -> BSL.ByteString -> IO ([FrameValue])
decodeFile tinfo contents = do
  let -- lazy = BSL.fromChunks [contents]
      length = DBG.runGet deserialiseHeader contents
  case length of
    Nothing -> do putStrLn "Invalid file format"
                  return []
    Just len_ -> do
      -- split up the string into three sections:
      -- (1) the 8 byte header we just read
      -- (2) the json blob
      -- (3) the binary frames
      let len = fromIntegral len_
          fileRecordBlob = BSL.take len $ BSL.drop 8 contents
          binaryFrames = BSL.drop ((fromIntegral len_) + 8) contents
          mfileRecord = decode fileRecordBlob :: Maybe FileRecord
      case mfileRecord of
        Nothing -> do putStrLn $ "Trying to decode " ++ show len ++ " bytes."
                      putStrLn $ "Couldn't decode file metadata: " ++ show mfileRecord
                      putStrLn $ "Raw: "
                      BSLC.putStrLn fileRecordBlob
                      return []
        Just fileRecord -> do putStrLn $ "first " ++ (show $ 8 + len) ++ " bytes taken by header.  Remaining " ++ (
                                show $ BSL.length binaryFrames) ++ " left for file."
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
