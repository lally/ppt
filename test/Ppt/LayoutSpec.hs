module Ppt.LayoutSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Ppt.Frame.Layout
import Ppt.Frame.ParsedRep
import Ppt.ParserGen

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

x64 = TargetInfo 8 4 4 8 8 1

prop_getsLayedOut :: Frame -> Bool
prop_getsLayedOut fr =
  let res = compileFrames' x64 [fr] in
  case res of
    Left _ -> False
    Right _ -> True

prop_frameLayoutIsAligned :: Frame -> Bool
prop_frameLayoutIsAligned fr =
  all isAligned members
  where (Right [FLayout _ f members]) = compileFrames' x64 [fr]
        isAligned m =
          let sz = sizeOf x64 $ lType m in
          (lKind m == LKPadding (lSize m) || (lSize m == sz)) &&
          ((lOffset m) `mod` (lAlignment m) == 0)

prop_reasonablePaddingAmt :: Frame -> Bool
prop_reasonablePaddingAmt fr =
  (sum $ map paddingSz mems) <= (maximum $ map lSize mems)
  where
    (Right [FLayout _ _ mems]) = compileFrames' x64 [fr]
    paddingSz' (LKPadding n) = n
    paddingSz' _ = 0
    paddingSz mem = paddingSz' $ lKind mem

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
