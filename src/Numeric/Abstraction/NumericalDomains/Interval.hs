{-|
Module      : Numeric.Abstraction.NumericalDomains.Interval
Description : Definitions for numerical domains.
Copyright   : (c) Greg Anderson, 2020
License     : MIT
Maintainer  : ganderso@cs.utexas.edu
Stability   : experimental

Implementation of the integer domain.  -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numeric.Abstraction.NumericalDomains.Interval
  ( Bound(..)
  , Interval
  , mkInterval
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)

import Numeric.Abstraction
import Numeric.Abstraction.NumericalDomains.Data

-- | A bound is a real value or positive or negative infinity.
data Bound = NegInf | PosInf | Value Double deriving (Eq)

instance Show Bound where
  show NegInf = "-oo"
  show PosInf = "+oo"
  show (Value c) = show c

-- Determine whether a pair of bounds imposes any constraints
unconstrained :: (Bound, Bound) -> Bool
unconstrained (NegInf, PosInf) = True
unconstrained _                = False

-- Multiply a number by a bound
multBound :: Double -> Bound -> Bound
multBound c NegInf
  | c < 0     = PosInf
  | c == 0    = Value 0
  | otherwise = NegInf
multBound c PosInf
  | c < 0     = NegInf
  | c == 0    = Value 0
  | otherwise = PosInf
multBound c (Value x) = Value $ c * x

-- Multiply bounds by a constant
multiplyBounds :: Double -> (Bound, Bound) -> (Bound, Bound)
multiplyBounds x (l, u)
  | x < 0     = (multBound x u, multBound x l)
  | otherwise = (multBound x l, multBound x u)

-- Add two bounds. Note that the infinities in the first argument are assumed
-- to dominate infinities in the second argument, so this is not commutative.
addBound :: Bound -> Bound -> Bound
addBound NegInf    _         = NegInf
addBound PosInf    _         = PosInf
addBound _         NegInf    = NegInf
addBound _         PosInf    = PosInf
addBound (Value x) (Value y) = Value $ x + y

-- Add two sets of bounds.
addBounds :: (Bound, Bound) -> (Bound, Bound) -> (Bound, Bound)
addBounds (a, b) (c, d) = (addBound a c, addBound b d)

-- Choose the most restrictive set of bounds
narrowBounds :: (Bound, Bound) -> (Bound, Bound) -> (Bound, Bound)
narrowBounds (a, b) (c, d) = (max a c, min b d)

-- Choose the least restrictive set of bounds
widenBounds :: (Bound, Bound) -> (Bound, Bound) -> (Bound, Bound)
widenBounds (a, b) (c, d) = (min a c, max b d)

instance Ord Bound where
  compare NegInf    NegInf    = EQ
  compare NegInf    _         = LT
  compare _         NegInf    = GT
  compare PosInf    PosInf    = EQ
  compare PosInf    _         = GT
  compare _         PosInf    = LT
  compare (Value a) (Value b) = compare a b

-- | An interval represents a set of independent constraints on each dimension.
newtype Interval d = Interval (M.Map d (Bound, Bound))

itvTop :: Ord d => S.Set d -> Interval d
itvTop s = Interval $ S.foldl' (\m a ->
  M.insert a (NegInf, PosInf) m) M.empty s

itvBottom :: Ord d => S.Set d -> Interval d
itvBottom s = Interval $ S.foldl' (\m a ->
  M.insert a (Value 1, Value 0) m) M.empty s

itvJoin :: Ord d => Interval d -> Interval d -> Interval d
itvJoin (Interval m1) (Interval m2) = Interval $ M.unionWith widenBounds m1 m2

itvMeet :: Ord d => Interval d -> Interval d -> Interval d
itvMeet (Interval m1) (Interval m2) = Interval $ M.unionWith narrowBounds m1 m2

itvIsTop :: Ord d => Interval d -> Bool
itvIsTop (Interval m) = S.foldl' (\b a ->
    b && unconstrained (fromMaybe (NegInf, PosInf) $ M.lookup a m))
  True (M.keysSet m)
itvIsBottom :: Ord d => Interval d -> Bool
itvIsBottom (Interval m) = any (uncurry (>)) $ M.elems m

itvRemDims :: Ord d => S.Set d -> Interval d -> Interval d
itvRemDims s (Interval m1) =
    Interval $ S.foldl' (flip M.delete) m1 s

-- Assign a single affine transform to a variable.
itvAssign :: Ord d => d -> AffineTransform d -> Interval d -> Interval d
itvAssign d at (Interval m) =
  Interval $ M.update (Just . addC (affTransConst at) . const transform) d m
   where
     -- Choose the least restrictive set of bounds
    transform = foldlAffineTransform (\(l, u) k c ->
      case M.lookup k m of
        Nothing -> (NegInf, PosInf)
        Just (lb, ub) -> let (al, au) =
                               if c < 0 then (multBound c ub, multBound c lb)
                                        else (multBound c lb, multBound c ub)
                          in (addBound l al, addBound u au))
        (Value 0, Value 0) at
    addC c (l, u) = (addBound l $ Value c, addBound u $ Value c)

-- Assign a set of affine transforms in parallel.
itvParAssign :: Ord d => M.Map d (AffineTransform d) -> Interval d -> Interval d
itvParAssign ts i@(Interval m) =
  Interval $ M.mapWithKey
               (\k b -> case M.lookup k ts of
                          Nothing -> b
                          Just t -> let Interval m' = itvAssign k t i
                                     in fromJust $ M.lookup k m') m

-- High level strategy: up to N times or until fixpoint, update each dimension
-- with new bounds computed from the existing bounds. Each linear constraint
-- has the form c^T x <= b. In order to udpate dimension k, we have
-- c'^T x' - b \le -c_k x_k where c' and x' are c and x with the k'th dimension
-- removed. Then we can use known bounds on x' to establish bounds on
-- c'^T x' - b. From there we can establish new bounds x_k
itvConstrain :: Ord d => S.Set (LinearConstraint d) -> Interval d -> Interval d
itvConstrain cs = constrain' (0 :: Int) where
  constrain' n i =
    if n >= 10
       then i
       else let ni = oneStep i
             in if ni == i then i else constrain' (n + 1) ni
  oneStep (Interval m) =
    Interval $ M.mapWithKey (narrowBounds . collectBounds m) m
  -- Bound a particular dimension for all of the constraints.
  collectBounds m d =
    S.foldl narrowBounds (NegInf, PosInf) $ S.map (boundsWithHoldout m d) cs
  -- Find bounds on the held-out dimension for one constraint.
  boundsWithHoldout m d c =
    let (kl, _) = boundCXk m d c
     in case linConsCoeff c d of
          Nothing | kl <= Value 0 -> (NegInf, PosInf)
                  | otherwise     -> (Value 1, Value 0)
          -- [a, b] <= x v
          -- if x < 0 then [a, b] / x >= v and v <= a / x
          -- otherwise     [a, b] / x <= v and v >= a / x
          Just x | x == 0    -> if kl <= Value 0 then (NegInf, PosInf)
                                                 else (Value 1, Value 0)
                 | x < 0     -> (multBound (1 / (- x)) kl, PosInf)
                 | otherwise -> (NegInf, multBound (1 / (- x)) kl)
  -- Find a bound on c_k x_k
  boundCXk m d c = foldlLinearConstraint (\acc d' x ->
    if d == d'
       then acc
       else let bds = fromMaybe (NegInf, PosInf) $ M.lookup d' m
             in addBounds acc $ multiplyBounds x bds)
               (Value $ - (linConsConst c), Value $ - (linConsConst c)) c

instance Ord d => AbstractDomain d (Interval d) where
  top = itvTop
  bottom = itvBottom
  join = itvJoin
  meet = itvMeet
  isTop = itvIsTop
  isBottom = itvIsBottom
  removeVars = itvRemDims
  constrainedVars (Interval m) = M.keysSet m

instance Ord d => NumericalDomain d (Interval d) where
  assign = itvParAssign
  constrain = itvConstrain

instance Ord d => Eq (Interval d) where
  i1@(Interval m1) == i2@(Interval m2) =
    (isBottom i1 && isBottom i2)
    || S.foldl' (\acc k -> acc && boundsEqual k)
                True (S.union (M.keysSet m1) (M.keysSet m2)) where
      boundsEqual k = case M.lookup k m1 of
                        Nothing -> case M.lookup k m2 of
                                     Nothing -> True
                                     Just (NegInf, PosInf) -> True
                                     _ -> False
                        Just (NegInf, PosInf) -> case M.lookup k m2 of
                                                   Nothing -> True
                                                   Just (NegInf, PosInf) -> True
                                                   _ -> False
                        Just (l1, u1) -> case M.lookup k m2 of
                                           Nothing -> False
                                           Just (l2, u2) -> l1 == l2 && u1 == u2

instance (Ord d, Show d) => Show (Interval d) where
  show (Interval m) = intercalate ", " $ map showDimension $ M.keys m where
    showDimension x =
      let (a, b) = fromMaybe (NegInf, PosInf) $ M.lookup x m
       in show x ++ ": [" ++ show a ++ "," ++ show b ++ "]"

-- | Construct an interval given bounds on each variable.
mkInterval :: Ord d => M.Map d (Bound, Bound) -> Interval d
mkInterval = Interval

