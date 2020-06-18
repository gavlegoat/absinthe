module Numeric.Abstraction.NumericalDomainsSpec where

import Test.Hspec
import Numeric.Abstraction
import Numeric.Abstraction.NumericalDomains

import qualified Data.Set as S
import qualified Data.Map as M

testDims :: S.Set Integer
testDims = S.fromList [0, 1, 2]

topI :: Interval Integer
topI = top testDims

bottomI :: Interval Integer
bottomI = bottom testDims

itv1 :: Interval Integer
itv1 = makeInterval $ M.fromList
  [ (0, (NegInf , Value 2))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

itv2 :: Interval Integer
itv2 = makeInterval $ M.fromList
  [ (0, (NegInf , Value 2))
  , (1, (Value 1, Value 0))
  , (2, (Value 0, Value 4))
  ]

spec :: Spec
spec = do
  describe "top" $ do
    it "is top" $ do
      topI `shouldSatisfy` isTop
    it "is not bottom" $ do
      topI `shouldSatisfy` (not . isBottom)
    it "join anything is top" $ do
      topI `join` bottomI `shouldSatisfy` isTop
      topI `join` itv1 `shouldSatisfy` isTop
    it "meet anything is the other thing" $ do
      topI `meet` bottomI `shouldBe` bottomI
      topI `meet` itv1 `shouldBe` itv1
  describe "bottom" $ do
    it "is bottom" $ do
      bottomI `shouldSatisfy` isBottom
    it "is not top" $ do
      bottomI `shouldSatisfy` (not . isTop)
    it "join anything is the other thing" $ do
      bottomI `join` topI `shouldBe` topI
      bottomI `join` itv1 `shouldBe` itv1
    it "meet anything is bottom" $ do
      bottomI `meet` topI `shouldSatisfy` isBottom
      bottomI `meet` itv1 `shouldSatisfy` isBottom
  describe "isTop" $ do
    it "is only true for top" $ do
      itv1 `shouldSatisfy` (not . isTop)
  describe "isBottom" $ do
    it "is only true for bottom" $ do
      itv1 `shouldSatisfy` (not . isBottom)
    it "is true whenever any constraint is impossible" $ do
      itv2 `shouldSatisfy` isBottom
  describe "removeDimensions" $ do
    it "should remove the right dimensions" $ do
      constrainedDims (removeDimensions (S.fromList [0, 1]) itv1) `shouldBe` S.fromList [2]

main :: IO ()
main = hspec spec
