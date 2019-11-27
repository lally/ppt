module Ppt.LayoutSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Ppt.Frame.Types
import Ppt.Frame.Layout
import Ppt.Frame.LayoutAlgo
import Ppt.Frame.ParsedRep
import Ppt.Generate.Cp
import Ppt.Generate.CpConfig
import Ppt.Generate.CpPrim
import Ppt.ParserGen

import Control.Lens

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- |Tests that it gets laid out.
prop_getsLayedOut :: Frame -> Bool
prop_getsLayedOut fr =
  let res = compileFrames' x64Layout [fr] in
  case res of
    Left _ -> False
    Right _ -> True

-- |Tests that the members are all properly aligned.
prop_frameLayoutIsAligned :: Frame -> Bool
prop_frameLayoutIsAligned fr =
  all isAligned members
  where (Right [FLayout _ f members]) = compileFrames' x64Layout [fr]
        isAligned m =
          let sz = sizeOf x64Layout $ m ^. lType in
          (m ^. lKind == LKPadding (m ^. lSize) || (m ^. lSize == sz)) &&
          ((m ^. lOffset) `mod` (m ^. lAlignment) == 0)

-- |Tests that the total padding is < half the total size. A poor test.
prop_reasonablePaddingAmt :: Frame -> Bool
prop_reasonablePaddingAmt fr =
  (sum $ map paddingSz mems) < 2 * (maximum $ mems ^..folded.lSize)
  where
    (Right [FLayout _ _ mems]) = compileFrames' x64Layout [fr]
    paddingSz' (LKPadding n) = n
    paddingSz' _ = 0
    paddingSz mem = paddingSz' $ mem ^. lKind

-- |Split the frame padding into sections by size and type, and make
-- sure each sections' padding is correct.
prop_minimalPaddingAmt :: Frame -> Bool
prop_minimalPaddingAmt fr = True

-- This is stupid, there should be a simpler way for doing this.
allEql :: (Eq a) => [a] -> Bool
allEql [] = True
allEql [x] = True
allEql (x:xs) = all id $ map (== x) xs

-- |If any has a discriminiator, they all should.
hasDiscriminators elems =
  all hasDiscriminator layouts
  where (Right layouts) = compileFrames' x64Layout elems
        hasDiscriminator (FLayout _ _ mems) =
          any isDiscrim (mems ^..folded.lKind) where
          isDiscrim (LKTypeDescrim _) = True
          isDiscrim _ = False

-- |Whether they're all the same size.
sameSize elems =
  allEql $ map sizeOf layouts
  where (Right layouts) = compileFrames' x64Layout elems
        sizeOf layout = (\m -> m ^. lSize + m ^. lOffset) $ last $ layout ^. flLayout

-- |Whether the offsets of the front seqno, back seqno, and discriminiator are the same.
sameOffsets elems =
  allEql $ map metaOffsets layouts
  where (Right layouts) = compileFrames' x64Layout elems
        metaOffsets layout = (frontSeqno, backSeqno, descrim)
          where mems = layout ^. flLayout
                frontSeqno = view lOffset $ head $ filter (\m -> m ^. lKind == (LKSeqno FrontSeq)) mems
                backSeqno = view lOffset $ head $ filter (\m -> m ^. lKind == (LKSeqno BackSeq)) mems
                isDiscrim (LKTypeDescrim _) = True
                isDiscrim _ = False
                descrim = view lOffset $ head $ filter (\m -> isDiscrim $ m ^. lKind) mems

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

