{-# LANGUAGE DeriveGeneric #-}
module Ppt.Frame.Layout where
import GHC.Generics
import Data.Aeson
import Ppt.Frame.ParsedRep
import Data.List

-- |Indicates sizes of machine types on the runtime platform.  Also
-- includes other layout-related values due to options.
data TargetInfo = TargetInfo { tDouble :: Int
                             , tFloat :: Int
                             , tInt :: Int
                             , tTime :: Int
                             , tCounter :: Int
                             , tPadTo :: Int
                               -- ^Minimum size to pad an object to.
                               -- Normally the same as tInt, unless
                               -- there's an option to pad it higher,
                               -- to take advantage of specialized
                               -- instructions.
                             } deriving (Generic, Eq, Show)

-- |Machine Layout
data SeqNoSide = FrontSeq | BackSeq deriving (Generic, Eq, Show)
data LayoutKind = LKSeqno SeqNoSide
                  -- ^Sequence numbers.  Front and back
                | LKTypeDescrim Int
                  -- ^Number of elements to discriminate by.
                | LKMember FrameElement (Maybe (Int,Int))
                  -- ^An actual declared member.  It may be one of a
                  -- group.  (Current, Total), where Current < Total,
                  -- always.
                 | LKPadding Int
                   -- ^Padding between members, or at end.  Param is
                   -- number of bytes
                deriving (Generic, Eq, Show)

data LayoutMember = LMember { lType :: Primitive
                            , lOffset :: Int
                            , lAlignment :: Int
                              -- ^Typically the same as lSize, unless it's padding
                            , lSize :: Int
                              -- ^Purely a function of TargetInfo and lType
                            , lKind :: LayoutKind
                            , lName :: String }
                   deriving (Generic, Eq, Show)

data FrameLayout = FLayout String Frame [LayoutMember] deriving (Generic, Eq, Show)

sizeOf :: TargetInfo -> Primitive -> Int
sizeOf info PDouble = tDouble info
sizeOf info PFloat = tFloat info
sizeOf info PInt = tInt info
sizeOf info PTime = tTime info
sizeOf info PCounter = tTime info

serializeOffsets :: TargetInfo -> [LayoutMember] -> [LayoutMember]
serializeOffsets target inmems =
  let serialize :: TargetInfo -> Int -> [LayoutMember] -> [LayoutMember]
      serialize tinfo pfx (m:mems) =
        let nextElemSize = pfx + (sizeOf tinfo (lType m))
        in (m {lOffset = pfx }):(serialize tinfo nextElemSize mems)
  in serialize target 0 inmems

{-
  Things to consider:
   - The layout process has to insert:
     - The front/back sequence numbers
     - The type descriminator
   - The layout representation has to wrap up the type descriminator too.

  Layout Process
  --------------
   1. Create initial LayoutMembers.
   2. Fill in their sizes.  Determine Alignment.
   3. Insert Front Sequence numbers and (optionally) discriminator
   4. Sort for alignment-base packing
   5. Add padding and back sequence number.

-}

diffName :: String -> Int -> String
diffName nm 0 = nm ++ "__start"
diffName nm 1 = nm ++ "__end"
diffName _ _ = undefined

-- |Initial layout information for a given FrameElement.  This is stages 1-2.
baseLayout :: TargetInfo -> FrameElement -> [LayoutMember]
baseLayout target elem@(FMemberElem (FMember ty nm True)) =
  let sz = sizeOf target ty
      elm = \n -> LKMember elem (Just (n,2))
  in [
    LMember ty 0 sz sz (elm 0) (diffName nm 0),
    LMember ty 0 sz sz (elm 1) (diffName nm 1)
   ]
baseLayout target elem@(FMemberElem (FMember ty nm False)) =
  let sz = sizeOf target ty
      elm = LKMember elem Nothing
  in [LMember ty 0 sz sz elm nm]
baseLayout _ (FCalculatedElem _ _ _ _ _) = []

frontSeqnoName = "__ppt_seqno"
descrimName = "__ppt_type"
backPadding = "__ppt_pad"

addPrefixes :: TargetInfo -> [FrameLayout] -> [FrameLayout]
addPrefixes _ [] = []
addPrefixes tinfo [single@(FLayout n f mems)] =
  let seqnSz = tInt tinfo
  in [FLayout n f ((LMember PInt 0 seqnSz seqnSz (LKSeqno FrontSeq)
                    frontSeqnoName):mems)]
addPrefixes tinfo multiple =
  let addDescrim = undefined
      seqnSz = tInt tinfo
      descSz = seqnSz -- TODO: resize this to be ceil_8(log_2(length multiple))
  in map (\(FLayout n f mems) -> FLayout n f ([
             LMember PInt 0 seqnSz seqnSz (LKSeqno FrontSeq) frontSeqnoName,
             LMember PInt 0 descSz descSz (LKTypeDescrim descSz) descrimName
             ] ++ mems)) multiple

-- |Removes the earliest set of items s.t. (sum $ map fn items). <=
-- total.  Returns ([sum set], [remainder]).  Only works well if the
-- input is sorted in descending order.
takeSum :: (Num n, Ord n)  => n -> (a -> n) -> [a] -> ([a], [a])
takeSum desiredTotal func elements =
  let takeSum' 0 _ xs (taken, rem) = (taken, rem ++ xs)
      takeSum' total fn [] (taken, rem) = (taken, rem)
      takeSum' total fn (x:xs) (taken, rem) =
        let val = fn x
        in if val <= total
           then takeSum' (total - val) fn xs (taken ++ [x], rem)
           else takeSum' total fn xs (taken, rem ++ [x])
  in takeSum' desiredTotal func elements ([], [])

alignRemainder :: Int -> Int -> Int
alignRemainder align amt
  | align == amt = 0
  | otherwise = align - (amt `mod` align)

-- |Compacts a FrameLayout by sorting the members to fill all
-- available space.  Algorithm: We're filling in blocks of size
-- 'align' bytes each.  So, scan for the LayoutMembers that aren't
-- FrameElements (like sequence numbers) at the front of the list.
-- See how many blocks we fill that way.  If we need to fill the last
-- block, scan the member list in sorted order, and try to find a set
-- of them that fit the size.
sortMembers :: FrameLayout -> FrameLayout
sortMembers (FLayout n f mems) =
  let align :: Int
      align = maximum $ map lSize mems
      -- Generally we use lSize and align to the largest size.
      -- lAlignment /= lSize only when we're doing padding.
      isMember (LKMember _ _) = True
      isMember _ = False
      (front, restRaw) = break (isMember . lKind)  mems
      frontSz = sum $ map lSize front
      frontRem = alignRemainder align frontSz
      rest = reverse $ sortOn lSize restRaw
      restMax = lSize $ head rest
  in if (length rest) > 0
     then let (frontRest, remainder) = takeSum frontRem lSize rest
              prepaddedFront = front ++ frontRest
              paddedFront =
                let pad = alignRemainder (
                      sum $ map lSize prepaddedFront) align
                in if pad == 0
                   then prepaddedFront
                   else prepaddedFront ++ [
                  LMember PByte 0 1 pad (LKPadding pad) backPadding
                  ]
          in FLayout n f paddedFront
     else FLayout n f mems

-- Lay out frames
compileFrames :: TargetInfo -> EmitOptions -> [Frame] -> FrameLayout
compileFrames target (Emit _ _  timerep runtime _) frs =
  let baseFrame target fr@(Frame nm elems)=
        FLayout nm fr (concatMap (baseLayout target) elems)
      -- frames before we equalize their sizes and add the back seqno.
      prepad = map sortMembers $ addPrefixes target $ map (baseFrame target) frs

  in sortMembers $ undefined


-- Then get theem padded to the same size.
instance ToJSON SeqNoSide
instance ToJSON LayoutKind
instance ToJSON LayoutMember
instance ToJSON FrameLayout
instance ToJSON TargetInfo

instance FromJSON SeqNoSide
instance FromJSON LayoutKind
instance FromJSON LayoutMember
instance FromJSON FrameLayout
instance FromJSON TargetInfo
