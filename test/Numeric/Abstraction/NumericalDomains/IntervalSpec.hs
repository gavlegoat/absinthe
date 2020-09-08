module Numeric.Abstraction.NumericalDomains.IntervalSpec where

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
itv1 = mkInterval $ M.fromList
  [ (0, (NegInf , Value 2))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

itv2 :: Interval Integer
itv2 = mkInterval $ M.fromList
  [ (0, (NegInf , Value 2))
  , (1, (Value 1, Value 0))
  , (2, (Value 0, Value 4))
  ]

zeroTrans :: AffineTransform Integer
zeroTrans = mkAffineTransform (M.fromList []) 0

itvZero :: Interval Integer
itvZero = mkInterval $ M.fromList
  [ (0, (Value 0, Value 0))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

allTrans :: AffineTransform Integer
allTrans = mkAffineTransform (M.fromList [(0, 1), (1, 1), (2, 1)]) 0

itvUnbound :: Interval Integer
itvUnbound = mkInterval $ M.fromList
  [ (0, (NegInf , PosInf ))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

midTrans :: AffineTransform Integer
midTrans = mkAffineTransform (M.fromList [(0, 1), (2, 1)]) 2

itvMid :: Interval Integer
itvMid = mkInterval $ M.fromList
  [ (0, (NegInf , Value 8))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

negTrans :: AffineTransform Integer
negTrans = mkAffineTransform (M.fromList [(1, -1), (2, -1)]) (-1)

itvNeg :: Interval Integer
itvNeg = mkInterval $ M.fromList
  [ (0, (NegInf , Value (-2)))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

itv3 :: Interval Integer
itv3 = mkInterval $ M.fromList
  [ (0, (Value 1, Value 3))
  , (1, (Value 1, Value 4))
  , (2, (Value 0, Value 4))
  ]

joinRes :: Interval Integer
joinRes = mkInterval $ M.fromList
  [ (0, (NegInf , Value 3))
  , (1, (Value 1, PosInf ))
  , (2, (Value 0, Value 4))
  ]

meetRes :: Interval Integer
meetRes = mkInterval $ M.fromList
  [ (0, (Value 1, Value 2))
  , (1, (Value 1, Value 4))
  , (2, (Value 0, Value 4))
  ]

linConsUnsat :: LinearConstraint Integer
linConsUnsat = mkLinearConstraint (M.fromList []) (-1)

linCons1 :: LinearConstraint Integer
linCons1 = mkLinearConstraint (M.fromList [(0, 1)]) 1

lc1 :: Interval Integer
lc1 = mkInterval $ M.fromList
  [ (0, (Value 1, Value 1))
  , (1, (Value 1, Value 4))
  , (2, (Value 0, Value 4))
  ]

linCons2 :: S.Set (LinearConstraint Integer)
linCons2 = S.fromList $
  [ mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 1
  , mkLinearConstraint (M.fromList [(2, -1)]) (-1)
  ]

itv4 :: Interval Integer
itv4 = mkInterval $ M.fromList
  [ (0, (Value 0, Value 2))
  , (1, (Value 0, Value 2))
  , (2, (Value 0, Value 2))
  ]

lc2 :: Interval Integer
lc2 = mkInterval $ M.fromList
  [ (0, (Value 0, Value 1))
  , (1, (Value 0, Value 1))
  , (2, (Value 1, Value 2))
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

  describe "equal" $ do
    it "should be reflexive" $ do
      topI `shouldBe` topI
      bottomI `shouldBe` bottomI
      itv1 `shouldBe` itv1

  describe "isTop" $ do
    it "is only true for top" $ do
      itv1 `shouldSatisfy` (not . isTop)

  describe "isBottom" $ do
    it "is only true for bottom" $ do
      itv1 `shouldSatisfy` (not . isBottom)
    it "is true whenever any constraint is impossible" $ do
      itv2 `shouldSatisfy` isBottom

  describe "join" $ do
    it "should work in normal circumstances" $ do
      join itv1 itv3 `shouldBe` joinRes

  describe "meet" $ do
    it "should work in normal cases" $ do
      meet itv1 itv3 `shouldBe` meetRes

  describe "removeVars" $ do
    it "should remove the right variables" $ do
      constrainedVars (removeVars (S.fromList [0, 1]) itv1) `shouldBe`
          S.fromList [2]

  describe "assign zero" $ do
    it "should yield zero" $
      assign (M.singleton 0 zeroTrans) itv1 `shouldBe` itvZero

  describe "assign unbounded" $ do
    it "should give an unbounded dimension" $ do
      assign (M.singleton 0 allTrans) itv1 `shouldBe` itvUnbound
    it "should go to top" $ do
      assign (M.fromList [(0, allTrans), (1, allTrans), (2, allTrans)]) itv1
          `shouldSatisfy` isTop
      assign (M.fromList [(0, allTrans), (1, allTrans), (2, allTrans)]) itv1
          `shouldBe` topI

  describe "assign" $ do
    it "should work for a normal case" $ do
      assign (M.singleton 0 midTrans) itv1 `shouldBe` itvMid
    it "should work with negative coefficients" $ do
      assign (M.singleton 0 negTrans) itv1 `shouldBe` itvNeg

  describe "constrain" $ do
    it "with unsat is bottom" $ do
      constrain (S.singleton linConsUnsat) itv1 `shouldSatisfy` isBottom
    it "with no constraints is identity" $ do
      constrain S.empty itv1 `shouldBe` itv1
      constrain S.empty bottomI `shouldBe` bottomI
      constrain S.empty itvZero `shouldBe` itvZero
    it "should behave reasonably for normal cases" $ do
      constrain (S.singleton linCons1) itv3 `shouldBe` lc1
      constrain linCons2 itv4 `shouldBe` lc2

main :: IO ()
main = hspec spec
