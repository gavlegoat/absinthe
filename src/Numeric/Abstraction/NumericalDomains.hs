{-|
Module      : Numeric.Abstraction
Description : Definitions for the interval domain
Copyright   : (c) Greg Anderson, 2020
License     : MIT Maintainer  : ganderso@cs.utexas.edu
Stability   : experimental

This module defines the abstract domain along with several useful functions.  -} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numeric.Abstraction.NumericalDomains
  ( Bound(..)
  , Interval
  , makeInterval
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Numeric.Abstraction

-- A 'Bound' is a real value or infinity.
data Bound = NegInf | PosInf | Value Double
  deriving (Eq)

instance Show Bound where
  show NegInf = "-oo"
  show PosInf = "+oo"
  show (Value c) = show c

unconstrained :: (Bound, Bound) -> Bool
unconstrained (NegInf, PosInf) = True
unconstrained _                = False

instance Ord Bound where
  compare NegInf    NegInf    = EQ
  compare NegInf    _         = LT
  compare _         NegInf    = GT
  compare PosInf    PosInf    = EQ
  compare PosInf    _         = GT
  compare _         PosInf    = LT
  compare (Value a) (Value b) = compare a b

-- |An interval represents a set of independent constraints on each dimension.
data Interval d = Interval (S.Set d) (M.Map d (Bound, Bound))
  deriving (Eq)

instance Ord d => AbstractDomain d (Interval d) where
  top s = Interval s $ S.foldl' (\m a -> M.insert a (NegInf, PosInf) m) M.empty s
  bottom s = Interval s $ S.foldl' (\m a -> M.insert a (Value 1, Value 0) m) M.empty s
  join (Interval s1 m1) (Interval s2 m2) =
    Interval (S.union s1 s2) $ M.unionWith (\(a, b) (c, d) -> (min a c, max b d)) m1 m2
  meet (Interval s1 m1) (Interval s2 m2) =
    Interval (S.union s1 s2) $ M.unionWith (\(a, b) (c, d) -> (max a c, min b d)) m1 m2
  isTop (Interval s m) =
    S.foldl' (\b a -> b && (unconstrained $ fromMaybe (NegInf, PosInf) $ M.lookup a m)) True s
  isBottom (Interval _ m) = any (uncurry (>)) $ M.elems m
  removeDimensions s (Interval s1 m1) =
    Interval (S.difference s1 s) $ S.foldl' (flip M.delete) m1 s
  constrainedDims (Interval s _) = s

instance (Ord d, Show d) => Show (Interval d) where
  show (Interval s m) = intercalate ", " $ map showDimension $ S.toList s where
    showDimension x =
      let (a, b) = fromMaybe (NegInf, PosInf) $ M.lookup x m
       in show x ++ ": [" ++ show a ++ "," ++ show b ++ "]"

makeInterval :: Ord d => M.Map d (Bound, Bound) -> Interval d
makeInterval m = Interval (M.keysSet m) m
