module Numeric.Abstraction.NumericalDomains.OctagonSpec where

import Test.Hspec
import Numeric.Abstraction
import Numeric.Abstraction.NumericalDomains

import qualified Data.Set as S
import qualified Data.Map as M

testDims :: S.Set Integer
testDims = S.fromList [0, 1]

topO :: Octagon Integer
topO = top testDims

bottomO :: Octagon Integer
bottomO = bottom testDims

-- A normal closed octagon
oct1 :: Octagon Integer
oct1 = fromBounds (S.fromList [0, 1]) (M.fromList [(0, 0), (1, -1)])
                  (M.fromList [(0, 2), (1, 2)])

-- An unsatisfiable octagon
oct2 :: Octagon Integer
oct2 = fromBounds (S.fromList [0, 1]) (M.singleton 0 1) (M.singleton 0 0)

-- An open octagon
oct3 :: Octagon Integer
oct3 = fromConstraints
         (S.fromList [0, 1])
         (S.fromList [ mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 3
                     , mkLinearConstraint (M.fromList [(0, -1), (1, 1)]) 1
                     ])

--   -1 0 1 2 3
--  3 . . . . .
--  2 . +---+ .
--  1 . |/.\| .
--  0 ./| . |\.
-- -1 / +---+ \
-- -2 . . . . .

-- oct1 `join` oct3
joinRes :: Octagon Integer
joinRes = fromConstraints
            (S.fromList [0, 1])
            (S.fromList [ mkLinearConstraint (M.fromList [(0, 0), (1, 1)]) 2
                        , mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 4
                        , mkLinearConstraint (M.fromList [(0, -1), (1, 1)]) 2
                        ])

-- oct1 `meet` oct3
meetRes :: Octagon Integer
meetRes = fromConstraints
            (S.fromList [0, 1])
            (S.fromList [ mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 3
                        , mkLinearConstraint (M.fromList [(0, -1), (1, 1)]) 1
                        , mkLinearConstraint (M.fromList [(0, 1), (1, 0)]) 2
                        , mkLinearConstraint (M.fromList [(0, -1), (1, 0)]) 0
                        , mkLinearConstraint (M.fromList [(0, 0), (1, -1)]) 1
                        ])

zeroTrans :: AffineTransform Integer
zeroTrans = mkAffineTransform (M.fromList []) 0

-- oct1[x_0 := 0]
octZero :: Octagon Integer
octZero = fromBounds
            (S.fromList [0, 1])
            (M.fromList [(0, 0), (1, -1)])
            (M.fromList [(0, 0), (1, 2)])

allTrans :: AffineTransform Integer
allTrans = mkAffineTransform (M.fromList [(0, 1), (1, 1)]) 0

octUnbound :: Octagon Integer
octUnbound = fromBounds
               (S.fromList [0, 1])
               (M.fromList [(1, -1)])
               (M.fromList [(1, 2)])

removeOct :: Octagon Integer
removeOct = fromBounds (S.singleton 1) (M.singleton 1 (-1)) (M.singleton 1 2)

negTrans :: AffineTransform Integer
negTrans = mkAffineTransform (M.fromList [(1, -1)]) (-1)

-- oct1[x_0 := negTrans]
-- x' := x - y - 1, a <= x <= b, c <== y <= d
-- c <= y <= d stays the same.
-- x - 1 = x' + y -> a - 1 <= x' + y <= b - 1
octNeg :: Octagon Integer
octNeg = fromConstraints
           (S.fromList [0, 1])
           (S.fromList [ mkLinearConstraint (M.singleton 1 1) 2
                       , mkLinearConstraint (M.singleton 1 (-1)) 1
                       , mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 1
                       , mkLinearConstraint (M.fromList [(0, -1), (1, -1)]) 1
                       ])

linConsUnsat :: LinearConstraint Integer
linConsUnsat = mkLinearConstraint (M.fromList []) (-1)

linCons1 :: LinearConstraint Integer
linCons1 = mkLinearConstraint (M.fromList [(0, 1)]) 1

-- oct3 `constraint` linCons1
lc1 :: Octagon Integer
lc1 = fromConstraints
        (S.fromList [0, 1])
        (S.fromList [ mkLinearConstraint (M.fromList [(0, 1)]) 1
                    , mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 3
                    , mkLinearConstraint (M.fromList [(0, -1), (1, 1)]) 1
                    ])

linCons2 :: S.Set (LinearConstraint Integer)
linCons2 = S.fromList
  [ mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 3
  , mkLinearConstraint (M.fromList [(0, 2), (1, -1)]) (-1)
  ]

-- 0 <= x <= 2, -1 <= y <= 2 and x + y <= 3 and 2 x - y <= -1
--   -2-1 0 1 2 3 4 5
--  5 + . . . + . . .
--  4 . + . ./. . . .
--  3 . . + + . . . .
--  2 . . +-+-+ . . .
--  1 . . + . + . . .
--  0 . ./| . | + . .
-- -1 . + +---+ . + .
-- -2 ./. . . . . . +
-- -3 + . . . . . . .

-- oct1 `constraint` linCons2
lc2 :: Octagon Integer
lc2 = fromConstraints
        (S.fromList [0, 1])
        (S.fromList [ mkLinearConstraint (M.singleton 1 (-1)) 1
                    , mkLinearConstraint (M.singleton 0 1) 2
                    , mkLinearConstraint (M.singleton 0 (-1)) 0
                    , mkLinearConstraint (M.fromList [(0, 1), (1, 1)]) 3
                    , mkLinearConstraint (M.fromList [(0, 1), (1, -1)]) 1.5
                    ])

spec :: Spec
spec = do
  describe "top" $ do
    it "is top" $ do
      topO `shouldSatisfy` isTop
    it "is not bottom" $ do
      topO `shouldSatisfy` (not . isBottom)
    it "join anything is top" $ do
      topO `join` bottomO `shouldSatisfy` isTop
      topO `join` oct1 `shouldSatisfy` isTop
    it "meet anything is the other thing" $ do
      topO `meet` bottomO `shouldBe` bottomO
      topO `meet` oct1 `shouldBe` oct1

  describe "bottom" $ do
    it "is bottom" $ do
      bottomO `shouldSatisfy` isBottom
    it "is not top" $ do
      bottomO `shouldSatisfy` (not . isTop)
    it "join anything is the other thing" $ do
      bottomO `join` topO `shouldBe` topO
      bottomO `join` oct1 `shouldBe` oct1
    it "meet anything is bottom" $ do
      bottomO `meet` topO `shouldSatisfy` isBottom
      bottomO `meet` oct1 `shouldSatisfy` isBottom

  describe "equal" $ do
    it "should be reflexive" $ do
      topO `shouldBe` topO
      bottomO `shouldBe` bottomO
      oct1 `shouldBe` oct1

  describe "isTop" $ do
    it "is only true for top" $ do
      oct1 `shouldSatisfy` (not . isTop)

  describe "isBottom" $ do
    it "is only true for bottom" $ do
      oct1 `shouldSatisfy` (not . isBottom)
    it "is true whenever the constraint set is unsatisfiable" $ do
      oct2 `shouldSatisfy` isBottom

  describe "join" $ do
    it "should work in normal circumstances" $ do
      join oct1 oct3 `shouldBe` joinRes

  describe "meet" $ do
    it "should work in normal cases" $ do
      meet oct1 oct3 `shouldBe` meetRes

  describe "removeVars" $ do
    it "should remove the right variables" $ do
      constrainedVars (removeVars (S.fromList [0]) oct1) `shouldBe`
          S.fromList [1]
    it "should leave the other constraints intact" $ do
      removeVars (S.singleton 0) oct1 `shouldBe` removeOct

  describe "assign zero" $ do
    it "should yield zero" $
      assign (M.singleton 0 zeroTrans) oct1 `shouldBe` octZero

  describe "assign unbounded" $ do
    it "should give an unbounded dimension" $ do
      assign (M.singleton 0 allTrans) oct1 `shouldBe` octUnbound

  describe "assign" $ do
    it "should work with negative coefficients" $ do
      assign (M.singleton 0 negTrans) oct1 `shouldBe` octNeg

  describe "constrain" $ do
    it "with unsat is bottom" $ do
      constrain (S.singleton linConsUnsat) oct1 `shouldSatisfy` isBottom
    it "with no constraints is identity" $ do
      constrain S.empty oct1 `shouldBe` oct1
      constrain S.empty bottomO `shouldBe` bottomO
      constrain S.empty octZero `shouldBe` octZero
    it "should behave reasonably for normal cases" $ do
      constrain (S.singleton linCons1) oct3 `shouldBe` lc1
      constrain linCons2 oct1 `shouldBe` lc2

main :: IO ()
main = hspec spec
