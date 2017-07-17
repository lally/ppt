module Ppt.Frame.LayoutAlgo where
import Ppt.Frame.Layout
import Ppt.Frame.ParsedRep
import Data.List as L
import Debug.Trace
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Either

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
  show fmb = ("FMBlock " ++ (_frName fmb) ++ " align " ++ (
                 show $ _frAlign fmb) ++ "  front = [\n\t" ++ (
    L.intercalate "\n\t" $ map showLMem (_frFront fmb)) ++ "\n]  elems = [\n\t" ++ (
    L.intercalate "\n\t" $ map showLMem (_frInternalElements fmb)) ++ "\n]  back = [\n\t" ++ (
    L.intercalate "\n\t" $ map showLMem (_frBack fmb)) ++ "\n]")
    where showLMem lmem = (lName lmem) ++ ": sz=" ++ (show $ lSize lmem) ++ ", aiign=" ++ (
            show $ lAlignment lmem) ++ ", ty=" ++ (show $ lType lmem) ++ ", kind=" ++ (
            show $ lKind lmem)

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

-- |Takes from the first list until the function returns False.  Then
-- returns the partition of where the function returned false.
repPartition :: ([a] -> Bool) -> [a] -> ([a], [a])
repPartition fn items =
  let taken = last $ takeWhile fn $ L.inits items
  in (taken, drop (length taken) items)
--  in cTake fn [] items

repPartitionList :: ([a] -> Bool) -> [a] -> [[a]]
repPartitionList fn items =
  let induce :: ([a] -> Bool) -> ([a], [a]) -> [[a]]
      induce fn (l, []) = [l]
      induce fn (l, r) = [l] ++ induce fn (repPartition fn r)
  in induce fn (repPartition fn items)

-- |Simple hill climber.  Tries to beat 'priorMetric' by running
-- permute on 'prior'.  Will try up to 'maxCnt' permutations.
induceWhile :: (Ord met, Integral met, Eq n, Num n) =>
               (t a -> met) ->
               (t a -> t a) ->
               met ->
               n ->
               t a ->
               t a
induceWhile score permute priorMetric 0 prior = prior
induceWhile score permute priorMetric maxCnt prior =
  let thisRound = permute prior
      thisMetric = score thisRound
  in if thisMetric >= priorMetric
     then induceWhile score permute thisMetric (maxCnt - 1 ) thisRound
     else prior

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
      rawSortedSz = sum $ map fst rawSorted {- listSize -}
      -- This works b/c any item that's bigger than align is a higher
      -- power of align.
      initialBlockSet = repPartitionList (isAligned align) rawSorted
      tryImprove vals = (last vals):(init vals)
      scoreFn l = scoreList (fromIntegral align) (((frontSz, frontItem):l) ++ [(backSz, backItem)])
      initialMetric = scoreFn rawSorted
  in induceWhile scoreFn tryImprove initialMetric (length rawSorted) rawSorted

layoutMember :: TargetInfo -> FrameMember -> [LayoutMember]
layoutMember tinfo fr@(FMember ty nm True) =
  case ty of
    (PCounter n) ->
      if n /= Nothing
      then fail "Got allocated PCounter during layout"
      else -- replicate this.
        concatMap (\i -> makePair (PCounter (Just i)) i 3) [0..2]
    _ -> makePair ty 0 1
  where sz = sizeOf tinfo ty
        algn = alignOf tinfo ty
        makePair t a b = [
          LMember t 0 algn sz (LKMember fr (Just (IntBegin a b))) (nm ++ pfx ++ "_start"),
          LMember t 0 algn sz (LKMember fr (Just (IntEnd a b))) (nm ++ pfx ++ "_end")]
          where pfx = if b > 1 then ("_" ++ show a) else ""

layoutMember tinfo fr@(FMember ty nm False) =
  [LMember ty 0 algn sz (LKMember fr Nothing) (nm)]
  where sz = sizeOf tinfo ty
        algn = alignOf tinfo ty

extractBlock :: TargetInfo -> Int -> Frame -> FrameMemberBlock
extractBlock tinfo nr f@(Frame n mems) =
  let seqName FrontSeq = "__ppt_seqno"
      seqName BackSeq = "__ppt_seqno_back"
      seqNo t = LMember PInt 0 (tInt tinfo) (tInt tinfo) (LKSeqno t) $ seqName t
      front = if nr > 1
              then (8, [seqNo FrontSeq, LMember PInt 0 (tInt tinfo) (tInt tinfo) (LKTypeDescrim nr)
                         "__ppt_type"])
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

-- |Add padding to make members fit their alignments..
interPad :: Int -> [LayoutMember] -> [LayoutMember]
interPad off [] =  []
interPad off (t:ts)
  | (off `mod` (lAlignment t)) == 0 =
    {-traceShow ("Proceeding: ", off, t) $ -} (t { lOffset = off }):(
      interPad (off + lSize t) ts)
  | otherwise =
    let algn = lAlignment t
        padAmt = algn - (off `mod` algn)
        padMem = LMember (PByte) off 1 padAmt (LKPadding padAmt) ("__pad_" ++ show off)
    in {-traceShow ("Padding by ", padAmt, off, t) $ -} padMem:(
      interPad (off + padAmt) (t:ts))

-- |The minimum length of the FrameMemberBlock, before we add the back seqno or padding.
minLen :: FrameMemberBlock -> Int
minLen fmb = sum $ map lSize $ interPad 0 ( _frFront fmb ++  _frInternalElements fmb)

determineSizes :: [FrameMemberBlock] -> Int
determineSizes mems = maximum $ map minLen mems

backPad :: FrameMemberBlock -> Int -> [LayoutMember]
backPad fmb desiredSz =
  let mems =interPad 0 (_frFront fmb ++ _frInternalElements fmb)
      sz = (\m -> lOffset m + lSize m) $ last mems
      deltaSz = desiredSz - sz
  in if deltaSz > 0
     then {-traceShow ("last mem:", last mems, " delta: ", deltaSz) $ -}[LMember PByte sz 1 deltaSz (LKPadding deltaSz) (
              "__ppt_endpad_" ++ show deltaSz)]
     else if deltaSz == 0
          then []
          else throw $ InternalFailureException $ "Failed to equalize " ++ show (fmb, desiredSz)

makeFLayout :: Int -> FrameMemberBlock -> Either String FrameLayout
makeFLayout sz fmb =
  Right $ (FLayout (_frName fmb) (_frFrame fmb) $ interPad 0 (_frFront fmb ++ _frInternalElements fmb
                                                         ++ (backPad fmb sz) ++ _frBack fmb))

-- Terminology: a member block is all of the members of a frame, in FrameMemberBlocks.
--  An alignment block list is a grouping of members into alignment-sized blocks.
compileFrames' :: TargetInfo -> [Frame] -> Either String [FrameLayout]
compileFrames' tinfo [] = Left "No frames to lay out"
compileFrames' tinfo frames =
  let frameLen = length frames
      memBlocks = map (extractBlock tinfo frameLen) frames
      blockPresize = determineSizes memBlocks
      blockFinalSize = (roundTo (tInt tinfo) blockPresize) + (tInt tinfo)
  in {-traceShow (tinfo, frames) $ -} sequence $ map (makeFLayout blockPresize) memBlocks {- sizedBlocks -}
