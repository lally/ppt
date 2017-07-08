module Ppt.LayoutSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Ppt.Frame.Layout
import Ppt.Frame.LayoutAlgo
import Ppt.Frame.ParsedRep
import Ppt.Generate.Cp
import Ppt.Generate.CpConfig
import Ppt.Generate.CpPrim
import Ppt.ParserGen

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- x64 = x64Layout -- TargetInfo 8 4 4 16 8 1

prop_getsLayedOut :: Frame -> Bool
prop_getsLayedOut fr =
  let res = compileFrames' x64Layout [fr] in
  case res of
    Left _ -> False
    Right _ -> True

prop_frameLayoutIsAligned :: Frame -> Bool
prop_frameLayoutIsAligned fr =
  all isAligned members
  where (Right [FLayout _ f members]) = compileFrames' x64Layout [fr]
        isAligned m =
          let sz = sizeOf x64Layout $ lType m in
          (lKind m == LKPadding (lSize m) || (lSize m == sz)) &&
          ((lOffset m) `mod` (lAlignment m) == 0)

prop_reasonablePaddingAmt :: Frame -> Bool
prop_reasonablePaddingAmt fr =
  (sum $ map paddingSz mems) < 2 * (maximum $ map lSize mems)
  where
    (Right [FLayout _ _ mems]) = compileFrames' x64Layout [fr]
    paddingSz' (LKPadding n) = n
    paddingSz' _ = 0
    paddingSz mem = paddingSz' $ lKind mem

allEql :: (Eq a) => [a] -> Bool
allEql [] = True
allEql [x] = True
allEql (x:xs) = all id $ map (== x) xs

hasDiscriminators elems =
  all hasDiscriminator layouts
  where (Right layouts) = compileFrames' x64Layout elems
        hasDiscriminator (FLayout _ _ mems) =
          any isDiscrim (map lKind mems) where
          isDiscrim (LKTypeDescrim _) = True
          isDiscrim _ = False

sameSize elems =
  allEql $ map sizeOf layouts
  where (Right layouts) = compileFrames' x64Layout elems
        sizeOf layout = (\m -> lSize m + lOffset m) $ last $ flLayout layout

sameOffsets elems =
  allEql $ map metaOffsets layouts
  where (Right layouts) = compileFrames' x64Layout elems
        metaOffsets layout = (frontSeqno, backSeqno, descrim)
          where mems = flLayout layout
                frontSeqno = lOffset $ head $ filter (\m -> lKind m == (LKSeqno FrontSeq)) mems
                backSeqno = lOffset $ head $ filter (\m -> lKind m == (LKSeqno BackSeq)) mems
                isDiscrim (LKTypeDescrim _) = True
                isDiscrim _ = False
                descrim = lOffset $ head $ filter (\m -> isDiscrim $ lKind m) mems

prop_sameSize :: (Frame, Frame) -> Bool
prop_sameSize (a, b) = sameSize [a,b]

prop_sameSize3 :: (Frame, Frame, Frame) -> Bool
prop_sameSize3 (a, b, c) = sameSize [a,b, c]

prop_hasDiscriminator :: (Frame, Frame) -> Bool
prop_hasDiscriminator (a, b) = hasDiscriminators [a,b]

prop_hasDiscriminator3 :: (Frame, Frame, Frame) -> Bool
prop_hasDiscriminator3 (a, b, c) = hasDiscriminators [a,b, c]

prop_sameOffsets :: (Frame, Frame) -> Bool
prop_sameOffsets (a, b) = sameOffsets [a,b]

prop_sameOffsets3 :: (Frame, Frame, Frame) -> Bool
prop_sameOffsets3 (a, b, c) = sameOffsets [a,b,c]

spec :: Spec
spec = do
  describe "meta" $ do
    it "can generate valid Frames" $ do
      property $ prop_getsLayedOut
  describe "layout padding" $ do
    it "properly marks offsets and sizes of each type" $ do
      property $ prop_frameLayoutIsAligned

    -- The total padding on a type should be <= 2*greatest size - 2*sizeof seqno
    -- And I should have tests for when it can be lower.
    it "doesn't overpad a type" $ do
      property $ prop_reasonablePaddingAmt
  describe "mult-frame buffers" $ do
    it "have type descriminators" $ do property $ prop_hasDiscriminator
    it "have type descriminators (x3)" $ do property $ prop_hasDiscriminator3
    it "have the same size" $ do property $ prop_sameSize
    it "have the same size (x3)" $ do property $ prop_sameSize3
    it "have the same offsets for shared members" $ do property $ prop_sameOffsets
    it "have the same offsets for shared members (x3)" $ do property $ prop_sameOffsets3

