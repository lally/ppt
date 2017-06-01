{-# LANGUAGE DeriveGeneric #-}
module Ppt.Frame.Layout where
import GHC.Generics
import Data.Aeson
import Ppt.Frame.ParsedRep
import Control.Applicative
import Control.Monad
import Data.Either
import Debug.Trace
import Data.List as L
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
                | LKMember FrameMember (Maybe (Int,Int))
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
sizeOf info PByte = 1

alignOf :: TargetInfo -> Primitive -> Int
alignOf info PDouble = tDouble info
alignOf info PFloat = tFloat info
alignOf info PInt = tInt info
alignOf info (PTime rep) = tCounter info
alignOf info PCounter = tCounter info
alignOf info PByte = 1

roundTo :: (Integral n) => n -> n -> n
roundTo align val
 | (val `mod` align) == 0 = val
 | otherwise = val + (align - (val `mod` align))

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
diffName nm n = nm ++ "_nr" ++ (show n)

-- |Initial layout information for a given FrameElement.  This is stages 1-2.
baseLayout :: TargetInfo -> FrameElement -> [LayoutMember]
baseLayout target (FMemberElem elem@(FMember ty nm True)) =
  let sz = sizeOf target ty
      elm = \n -> LKMember elem (Just (n,2))
  in [
    LMember ty 0 sz sz (elm 0) (diffName nm 0),
    LMember ty 0 sz sz (elm 1) (diffName nm 1)
   ]
baseLayout target (FMemberElem elem@(FMember ty nm False)) =
  let sz = sizeOf target ty
      elm = LKMember elem Nothing
  in [LMember ty 0 sz sz elm nm]
baseLayout _ (FCalculatedElem _ _ _ _ _) = []

frontSeqnoName = "__ppt_seqno"
descrimName = "__ppt_type"
frontPaddingName = "__ppt_pad_front"
backPaddingName = "__ppt_pad_back"
backSeqnoName = "__ppt_seqno_back"

lkName (LKSeqno FrontSeq) = frontSeqnoName
lkName (LKSeqno BackSeq) = backSeqnoName
lkName (LKTypeDescrim _) = descrimName
lkName (LKMember (FMember _ nm _)  _) = nm
lkName (LKPadding _) = "__ppt_pad" -- This has to be combined with a number.

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


-- |Intermediate type during layout.  We separate out the raw
-- FrameMembers that take up space in the buffer, and calculate the
-- alignment and paddings.
data FrameMemberBlock = FMBlock { _frInternalElements :: [LayoutMember]
                                -- ^The members present in the raw data
                                , _frCalculatedElements :: [FrameElement]
                                -- ^^Members calculated from raw data
                                , _frName :: String
                                -- ^Name of Frame
                                , _frFrame :: Frame
                                -- ^The Frame
                                , _frAlign :: Int
                                -- ^The alignment requirements of this Frame
                                , _frFront :: [LayoutMember]
                                -- ^Front matter (seqno and maybe a type descrim)
                                , _frBack :: [LayoutMember]
                                -- ^Back matter (seqno)
                                } deriving (Eq)

instance Show FrameMemberBlock where
  show fmb = ("FMBlock " ++ (_frName fmb) ++ " align " ++ (show $ _frAlign fmb) ++ "  elems = [\n\t" ++ (
    L.intercalate "\n\t" $ map showLMem (_frInternalElements fmb)) ++ "\n]  front = [\n\t" ++ (
    L.intercalate "\n\t" $ map showLMem (_frFront fmb)) ++ "\n]  back = [\n\t" ++ (
    L.intercalate "\n\t" $ map showLMem (_frBack fmb)) ++ "\n]")
    where showLMem lmem = (lName lmem) ++ ": sz=" ++ (show $ lSize lmem) ++ ", aiign=" ++ (
            show $ lAlignment lmem) ++ ", ty=" ++ (show $ lType lmem) ++ ", kind=" ++ (show $ lKind lmem)

-- Enhancement: look at sizes returned from calcBlockPadding and
-- while (the back size > align && the front size > sizeof last FrameMember)
--   move last FrameMember to the front.
-- What's the closed form of this?  Let's avoid a silly loop.
--
-- I think jus ttake up to N bytes of FrameMembers (from the front!)
-- whenever we can result in an overall reduction in padding.  That's
-- whenever we have the (align - tail size) > sizeof seqno.  So the
-- difference there can go up fron if it's also <= fron tpadding.  It
-- should only do the move if, after scanning for how much it can
-- take, it can reduce padding.

alignOfAll :: TargetInfo -> [FrameMember] -> Int
alignOfAll tinfo frames = maximum $ padSz:sizes
  where
    padSz = sizeOf tinfo PInt  -- for seqno / type descrim
    sizes = map ((alignOf tinfo) . fmType) frames

-- |Returns (Alignment, front pad bytes, back pad bytes.  Both cases
-- assume an Int seqno added on front and back.
-- NOTE: if we do the optimization above, this function has to then
-- take the order into account, or the optimizing function will just
-- have to recalculate, based off this original data.
calcBlockPadding :: TargetInfo ->
                    [FrameMember] ->
                    (Int, Int, Int)
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


-- |Takes from the first list until the function returns False.  Then
-- returns the partition of where the function returned false.
repPartition :: ([a] -> Bool) -> [a] -> ([a], [a])
repPartition fn items =
  let cTake fn passed [] = (passed, [])
      cTake fn passed rest@(r:rs) =
        if fn (passed ++ [r])
        then cTake fn (passed ++ [r]) rs
        else (passed, rest)
  in cTake fn [] items

repPartitionList :: ([a] -> Bool) -> [a] -> [[a]]
repPartitionList fn items =
  let induce :: ([a] -> Bool) -> ([a], [a]) -> [[a]]
      induce fn (l, []) = [l]
      induce fn (l, r) = [l] ++ induce fn (repPartition fn r)
  in induce fn (repPartition fn items)

-- Simple hill climber
induceWhile :: (Ord met, Integral met, Eq n, Num n) => (t a -> met) -> (t a -> t a) -> met -> n -> t a -> t a
induceWhile score permute priorMetric 0 prior = prior
induceWhile score permute priorMetric maxCnt prior =
  let thisRound = permute prior
      thisMetric = score thisRound
  in if thisMetric >= priorMetric
     then induceWhile score permute thisMetric (maxCnt - 1 ) thisRound
     else prior

listSize :: (Num n) => [(n, a)] -> n
listSize s = sum $ map fst s

isAligned :: (Num n, Ord n) => n -> [(n, a)] -> Bool
isAligned _ (_:[]) = True
isAligned align items = align >= (sum $ map fst items)

-- For fixed alignment, and a fixed set of objects, the relative
-- scores of this function are fine.  But break either of those
-- assumptions and there's a problem.
scoreBlocks :: (Ord n, Num n) => n -> [[(n, a)]] -> n
scoreBlocks align l = -align * (fromIntegral $ length l)

scoreList :: (Ord n, Num n) => n -> [(n, a)] -> n
scoreList align ls = scoreBlocks align $ repPartitionList (isAligned align) ls

-- |Descending sort by fst.
sortTagged ::(Ord n, Num n) => [(n, a)] ->  [(n, a)]
sortTagged = sortBy (\a b -> compare (fst b) (fst a))

-- Odd, we should sort these by size and measure by alignment.  The
-- two are different in the timestamp case.  As alignment is an
-- argument, this isn't too bad.  We have to tolerate items > align.
-- TODO: we'll have to adjust as the type 'a' here can be a list of
-- items because the front matter can be two items.
moduloFit  :: (Ord n, Integral n) => n -> (n, a) -> (n, a) -> [(n, a)] -> [(n, a)]
moduloFit align frontEnd backEnd internals =
  let (frontSz, frontItem) = frontEnd
      (backSz, backItem) = backEnd
      rawSorted = sortTagged internals
      rawSortedSz = listSize rawSorted
      -- This works b/c any item that's bigger than align is a higher
      -- power of align.
      initialBlockSet = repPartitionList (isAligned align) rawSorted
      tryImprove vals = (last vals):(init vals)
      scoreFn l = scoreList (fromIntegral align) (((frontSz, frontItem):l) ++ [(backSz, backItem)])
      initialMetric = scoreFn rawSorted
  in induceWhile scoreFn tryImprove initialMetric (length rawSorted) rawSorted

layoutMember :: TargetInfo -> FrameMember -> [LayoutMember]
layoutMember tinfo fr@(FMember ty nm True) =
  [LMember ty 0 algn sz (LKMember fr (Just (0,2))) (nm ++ "_start"),
   LMember ty 0 algn sz (LKMember fr (Just (1,2))) (nm ++ "_end")]
  where sz = sizeOf tinfo ty
        algn = alignOf tinfo ty
layoutMember tinfo fr@(FMember ty nm False) =
  [LMember ty 0 algn sz (LKMember fr Nothing) (nm)]
  where sz = sizeOf tinfo ty
        algn = alignOf tinfo ty

extractBlock :: TargetInfo -> Int -> Frame -> FrameMemberBlock
extractBlock tinfo nr f@(Frame n mems) =
  let seqName FrontSeq = "__seqno_front"
      seqName BackSeq = "__seqno_back"
      seqNo t = LMember PInt 0 (tInt tinfo) (tInt tinfo) (LKSeqno t) $ seqName t
      front = if nr > 1
              then (8, [seqNo FrontSeq, LMember PInt 0 (tInt tinfo) (tInt tinfo) (LKTypeDescrim nr) "__type_descrim"])
              else (4, [seqNo FrontSeq])
      back = (4, [seqNo BackSeq])
      (wrRealMems, calculated) = partition (\c -> case c of
                                                    FMemberElem _ -> True
                                                    otherwise -> False) mems
      realMems = map (\f -> case f of FMemberElem t -> t) wrRealMems
      -- Make sure we always have enough alignment for the front matter.
      align = maximum [alignOfAll tinfo realMems, tInt tinfo]
      reorderedMems = concatMap snd  $ moduloFit align front back $ map (
        \m -> (sizeOf tinfo (fmType m), layoutMember tinfo m)) realMems
  in FMBlock reorderedMems calculated n f align (snd front) (snd back)

-- |The minimum length of the FrameMemberBlock, before we add the back seqno or padding.
minLen :: FrameMemberBlock -> Int
minLen fmb = sum $ map lSize $ concat [ _frInternalElements fmb, _frFront fmb]

-- |Size of the first padding member in the list.
padSz memb =
  maybe 0 id (padOf $ _frBack memb)
  where
    padOf [] =  Nothing
    padOf ((LMember _ _ _ _ (LKPadding sz) _):_) = Just sz
    padOf (_:ts) = padOf ts


padFront memb = memb { _frFront = (_frFront memb) ++ frontPadding }
  where frontPad 0 = []
        frontPad n = [LMember PByte 0 1 n (LKPadding n) "__front_padding"]
        sz lmems = sum $ map lSize lmems
        align = _frAlign memb
        frontPaddingAmt =
          let front = last $ takeWhile (\s -> (sum $ map lSize s) <= align) $ map (
                \l -> (_frFront memb) ++ l) (L.inits $ _frInternalElements memb)
              frsz = {-traceShow ("padFront for ", front, sz front)-} (sz front)
          in if align /= frsz
             then align - (frsz `mod` align)
             else 0
        frontPadding = frontPad frontPaddingAmt

padBack bytes memb
  | bytes >= (4+ minLen memb) = memb { _frBack = backPadding ++ (_frBack memb) }
  where
    backPad 0 = []
    backPad n = [LMember PByte 0 1 n (LKPadding n) "__back_padding"]
    backPaddingAmt = bytes - (minLen memb) - padSz memb - 4
    backPadding = backPad backPaddingAmt

determineSizes :: [FrameMemberBlock] -> Int
determineSizes mems = maximum $ map minLen mems

interPad :: Int -> [LayoutMember] -> Either String [LayoutMember]
interPad off [] = Right []
interPad off (t:ts)
  | (off `mod` (lAlignment t)) == 0 =
    {-traceShow ("Proceeding: ", off, t) $ -}liftM2 mappend (Right [t { lOffset = off }])  (interPad (off + lSize t) ts)
  | otherwise =
    let algn = lAlignment t
        padAmt = algn - (off `mod` algn)
        padMem = LMember (PByte) off 1 padAmt (LKPadding padAmt) ("__pad_" ++ show off)
    in {-traceShow ("Padding by ", padAmt, off, t) $ -}liftM2 mappend (Right [padMem]) (interPad (off + padAmt) (t:ts))

makeFLayout :: FrameMemberBlock -> Either String FrameLayout
makeFLayout fmb =
  (FLayout (_frName fmb) (_frFrame fmb)) <$> interPad 0 (_frFront fmb ++ _frInternalElements fmb ++ _frBack fmb)

-- Terminology: a member block is all of the members of a frame, in FrameMemberBlocks.
--  An alignment block list is a grouping of members into alignment-sized blocks.
compileFrames' :: TargetInfo -> [Frame] -> Either String [FrameLayout]
compileFrames' tinfo [] = Left "No frames to lay out"
compileFrames' tinfo frames =
  let frameLen = length frames
      memBlocks = map ({-padFront .-} (extractBlock tinfo frameLen)) frames
      blockPresize = determineSizes memBlocks
      blockFinalSize = (roundTo (tInt tinfo) blockPresize) + (tInt tinfo)
--      sizedBlocks = map (padBack blockFinalSize) memBlocks
  in {-traceShow (tinfo, frames) $ -}sequence $ map makeFLayout memBlocks {- sizedBlocks -}

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
compileFrames :: TargetInfo -> [Frame] -> Either String [FrameLayout]
compileFrames target frs =
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

