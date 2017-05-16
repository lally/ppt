{-# LANGUAGE DeriveGeneric #-}
module Ppt.Frame.Layout where
import GHC.Generics
import Data.Aeson
import Ppt.Frame.ParsedRep
import Data.List
import qualified Data.HashMap.Strict as HM

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

timeSize tinfo ETimeVal = tTime tinfo
timeSize tinfo (ETimeSpec _) = 2 * tTime tinfo

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

data LayoutIOSpec = LayoutIO { lioSize :: Int
                             , lioBackOffset :: Int
                             } deriving (Eq, Show)

data FrameLayout = FLayout { flName :: String
                           , flFrame :: Frame
                           , flLayout :: [LayoutMember]
                           } deriving (Generic, Eq, Show)

data JsonRep = JsonRep { jsAbi :: String
                       , jsBufferEmit :: EmitOptions
                       , jsBufferFrames :: [FrameLayout]
                       , jsTags :: [(String, String)]
                       , jsMetadata :: [String]
                       } deriving (Generic, Eq, Show)

layoutSpec :: FrameLayout -> LayoutIOSpec
layoutSpec layout =
  let sumSize = sum $ map lSize $ flLayout layout
      lastMem = last $ flLayout layout
      backOff = case lastMem of
                  (LMember _ off _ _ (LKSeqno BackSeq) _) -> off
  in LayoutIO sumSize backOff

sizeOf :: TargetInfo -> Primitive -> Int
sizeOf info PDouble = tDouble info
sizeOf info PFloat = tFloat info
sizeOf info PInt = tInt info
sizeOf info (PTime rep) = timeSize info rep
sizeOf info PCounter = tCounter info

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
diffName nm 0 = nm ++ "_start"
diffName nm 1 = nm ++ "_end"
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
frontPaddingName = "__ppt_pad_front"
backPaddingName = "__ppt_pad_back"
backSeqnoName = "__ppt_seqno_back"

addPrefixes tinfo multiple =
  let addDescrim =
        if length multiple > 1
        then [LMember PInt 0 descSz descSz (LKTypeDescrim descSz) descrimName]
        else []
      seqnSz = tInt tinfo
      descSz = seqnSz -- TODO: resize this to be ceil_8(log_2(length multiple))
  in map (\(FLayout n f mems) -> FLayout n f ((
             LMember PInt 0 seqnSz seqnSz (LKSeqno FrontSeq) frontSeqnoName):
                                              (addDescrim ++ mems))) multiple

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


data FrameMemberBlock = FMBlock { _frInternalElements :: [FrameMember]
                                , _frCalculatedElements :: [FrameElement]
                                , _frName :: String
                                , _frFrame :: Frame
                                , _frAlign :: Int
                                , _frFrontPad :: Int
                                , _frBackPad :: Int
                                }

-- |Returns (Alignment, front pad bytes, back pad bytes.  Both cases
-- assume an Int seqno added on front and back.
calcBlockPadding :: TargetInfo -> [FrameMember] -> (Int, Int, Int)
calcBlockPadding tinfo frames =
  (align, frontPad, backPad)
  where
    padSz = sizeOf tinfo PInt
    sizes = map ((sizeOf tinfo) . fmType) frames
    align = maximum $ padSz:sizes
    frontPad = align - padSz
    lastAlignSz = (sum sizes) `mod` align
    -- The end is tricky. If we have enough space to fit the seequence
    -- number with the last 'align'-wide group of frame members, put
    -- it there.  Otherwise put it in its own align-wide group.
    backPad = if (align - lastAlignSz) >= padSz
              then align - lastAlignSz - padSz
              else (align - lastAlignSz) + frontPad

extractBlock :: TargetInfo -> Frame -> FrameMemberBlock
extractBlock tinfo f@(Frame n mems) =
  let (align, frontpd, backpd) = calcBlockPadding tinfo realMems
      (wrRealMems, calculated) = partition (\c -> case c of
                                                    FMemberElem _ -> True
                                                    otherwise -> False) mems
      realMems = map (\f -> case f of FMemberElem t -> t) wrRealMems
  in FMBlock realMems calculated n f align frontpd backpd

makeLayout :: TargetInfo -> FrameMemberBlock -> Either String FrameLayout
makeLayout tinfo block =
  let x=x
  in undefined

compileFrames' :: TargetInfo -> [Frame] ->
                 Either String [FrameLayout]
compileFrames' tinfo frames =
  let blocks = map (extractBlock tinfo) frames

  in undefined

-- |Compacts a FrameLayout by sorting the members to fill all
-- available space.  Algorithm: We're filling in blocks of size
-- 'align' bytes each.  So, scan for the LayoutMembers that aren't
-- FrameElements (like sequence numbers) at the front of the list.
-- See how many blocks we fill that way.  If we need to fill the last
-- block, scan the member list in sorted order, and try to find a set
-- of them that fit the size.
sortMembers :: TargetInfo -> FrameLayout -> FrameLayout
sortMembers tinfo (FLayout n f mems) =
  let align :: Int
      align = maximum $ map lSize mems
      -- Generally we use lSize and align to the largest size.
      -- lAlignment /= lSize only when we're doing padding.
      isMember (LKMember _ _) = True
      isMember _ = False
      (front, restRaw) = break (isMember . lKind) mems
      frontSz = sum $ map lSize front
      frontRem = alignRemainder align frontSz
      rest = sortOn (\m -> -1 * lSize m) restRaw
      restMax = lSize $ head rest
  in if (length rest) > 0
     then let (frontRest, remainder) = takeSum frontRem lSize rest
              prefilledFront = front ++ frontRest
              filledFront =
                let pad = alignRemainder (
                      sum $ map lSize prefilledFront) align
                in if pad == 0
                   then prefilledFront
                   else prefilledFront ++ [
                  LMember { lType = PByte
                          , lOffset = 0
                          , lAlignment = 1
                          , lSize = pad
                          , lKind = LKPadding pad
                          , lName = frontPaddingName }
                  ]
          in FLayout n f (filledFront ++ remainder)
     else FLayout n f mems

setOffsets :: [LayoutMember] -> Either String [LayoutMember]
setOffsets mems =
  offset 0 mems
  where offset _ [] = Right []
        offset start (x:xs) =
          if (start `mod` lAlignment x) /= 0
          then Left ("Wrong alignment (" ++ show start ++ ") for " ++ show x)
          else let rest = offset (start + lSize x) xs
               in case rest of
                    Left err -> rest
                    Right remain -> Right (
                      x { lOffset = start }:remain)


-- Lay out frames
compileFrames :: TargetInfo -> EmitOptions -> [Frame] ->
                 Either String [FrameLayout]
compileFrames target eOpts frs =
  let baseFrame target fr@(Frame nm elems) =
        FLayout nm fr (concatMap (baseLayout target) elems)
      frameSize ::FrameLayout -> Int
      frameSize layout = sum $ map lSize $ flLayout layout
      -- frames before we equalize their sizes and add the back seqno.
      prepad :: [FrameLayout]
      prepad = map (sortMembers target) $ addPrefixes target $ map (baseFrame target) frs
      maxSize = maximum $ map frameSize prepad
      -- Now figure out how much to pad to.  TODO: this is both
      -- affected by the alignment of the sequence number (the rest of
      -- each is already aligned) and the tPadTo value in the
      -- TargetInfo.
      padSizeTo = maxSize + (tInt target)
      addBack :: Int -> FrameLayout -> FrameLayout
      addBack size frame =
        let padAmount = alignRemainder size $ sum $ map lSize $ flLayout frame
            backSeq = LMember { lType = PInt
                              , lOffset = 0
                              , lAlignment = tInt target
                              , lSize = tInt target
                              , lKind = LKSeqno BackSeq
                              , lName = backSeqnoName }
        in if padAmount == 0
           then frame { flLayout = (flLayout frame) ++ [backSeq]}
           else frame { flLayout = (flLayout frame) ++ [
                          LMember { lType = PByte
                                  , lOffset = 0
                                  , lAlignment = 1
                                  , lSize = padAmount
                                  , lKind = LKPadding padAmount
                                  , lName = backPaddingName}
                          , backSeq]}
       -- LOOKUP: List monad for applicative use of setOffsets on list
       -- of all frames.
      withBacks = map (addBack padSizeTo) prepad
      foldOffsets :: [FrameLayout] -> Either String [FrameLayout]
      foldOffsets [] = Right []
      foldOffsets (x:xs) =
        let ret = setOffsets $ flLayout x
            children = foldOffsets xs
        in case children of
             Left _ -> children
             Right rs -> case ret of
               Left s -> Left s
               Right r -> Right ((x { flLayout = r }):rs)
  in foldOffsets withBacks

mlast :: [a] -> Maybe a
mlast [] = Nothing
mlast [x] = Just x
mlast (a:as) = mlast as

-- |Determine the single-member element size of the shared memory segment.  In units of 32-bit words.
frameSize :: JsonRep -> Maybe Int
frameSize json =
  let emit = jsBufferEmit json
      frames = jsBufferFrames json
  in do aFrame <- mlast frames
        lastMem <- mlast $ flLayout aFrame
        return (lOffset lastMem + lSize lastMem)

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

instance ToJSON JsonRep where
  toJSON j = object [ "abi" .= jsAbi j
                    , "tags" .= toJSON (
                        map (\(k,v) -> object [ "key" .= k, "value" .= v ]) $ jsTags j)
                    , "metadata" .= jsMetadata j
                    , "emit" .= jsBufferEmit j
                    , "frames" .= jsBufferFrames j
                    ]

instance FromJSON JsonRep where
  parseJSON (Object o) = do -- withObject "_json" $ \o -> do
    abi <- o .:? "abi" .!= "not found"
    emit <- o .: "emit"
    frames <- o .:? "frames" .!= []
    tags <- o .:? "tags" .!= []
    md <- o .:? "metadata" .!= []
    return $ JsonRep abi emit frames tags md


data FileRecord = FileRecord { frFormat    :: String
                             , frDate      :: String
                             , frComment   :: String
                             , frDeltaTime :: Int    -- in seconds
                             , frJson      :: JsonRep }
                  deriving (Generic, Eq, Show)
instance ToJSON FileRecord
instance FromJSON FileRecord

