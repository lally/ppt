module Ppt.Decode where

import Ppt.Frame.ParsedRep hiding (FValue)
import Ppt.Frame.Layout
import Data.Maybe
import Data.Vector.Storable ((!), (!?))
import qualified Data.Vector.Storable as V

data FrameElementValue = FEValue { _primValue :: PrimitiveValue
                                 , _frameMember :: FrameMember
                                 , _layoutMember :: LayoutMember }
                         deriving (Eq, Show)

data FrameValue = FValue { _frame :: Frame
                         , _values :: [FrameElementValue] }
                  deriving (Eq, Show)

mrequire :: Bool -> a -> Maybe a
mrequire True a = Just a
mrequire False _ = Nothing
findMember :: String -> FrameLayout -> Maybe FrameMember
findMember name layout = mrequire (length mems == 1) $ head mems
                         where (Frame _ elements) = flFrame layout
                               memberNamed s (FMemberElem mem@(FMember _ name _)) | (s == name) = Just mem
                                                                                  | otherwise = Nothing
                               memberNamed _ _ = Nothing
                               mems = mapMaybe (memberNamed name) elements

readMember :: [LayoutMember] -> FrameLayout -> (V.Vector Int, TargetInfo, Int) -> Maybe [FrameElementValue]
readMember [] _ _ = Just []
readMember (lmem:lmems) layout v@(vec, tinfo, startIdx) =
  case (lKind lmem) of
    (LKMember (FMemberElem elem) _) ->
      (:) <$> thisResult <*> (readMember lmems layout v)
        -- TODO: implement the rest of these.  Look at what I can do in Vector to read different types.
        -- this probably needs to move to IO (Maybe [FrameElementValue]) and use Ptr and cast to read
        -- what I want.
        where primValue = case (fmType elem) of
                PDouble -> Nothing
                PFloat -> Nothing
                PInt -> PVIntegral <$> vec !? (lIxOf lmem)
                PTime -> PVTime <$> vec !? (lIxOf lmem) <*> vec !? (1 + (lIxOf lmem))
                PCounter -> Nothing
                PByte -> Nothing
              thisResult = FEValue <$> primValue  <*> (findMember (lName lmem) layout) <*> pure lmem
              ixOf off = off `div` (tInt tinfo)
              lIxOf mem = startIdx + (ixOf $ lOffset mem)
    otherwise -> readMember lmems layout v

decodeFromBuffer :: TargetInfo -> V.Vector Int -> Int -> Int -> [FrameLayout] -> Maybe FrameValue
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
     mrequire (sz == size) "Frame sizes are equal"
     mrequire (sz + startIdx <= V.maxIndex vec) "Frame fits in vector"
     mrequire (lIxOf memFrontSeq == startIdx) "Frame seqno is first"
     startSeqno <- vec !? startIdx
     endSeqno <- vec !? (lIxOf memBackSeq)
     mrequire (startSeqno == endSeqno) "Sequence numbers match"
     frameType <- vec !? (lIxOf memTypeDesc)
     mrequire (frameType < nrFrameTypes) "Frame type is defined"
     let layout = layouts !! frameType
     -- Go applicative on list constructor for members
     members <- readMember (flLayout layout) layout (vec, tinfo, startIdx)
     return (FValue (flFrame layout) members)

