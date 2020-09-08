{-|
Module      : Numeric.Abstraction.NumericalDomains.Data
Description : Definitions for numerical domains.
Copyright   : (c) Greg Anderson, 2020
License     : MIT
Maintainer  : ganderso@cs.utexas.edu
Stability   : experimental

This module defines abstract domains over numerical, real-valued, variables.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Numeric.Abstraction.NumericalDomains.Data
  ( AffineTransform
  , mkAffineTransform
  , affTransCoeff
  , affTransConst
  , addVarTrans
  , subVarTrans
  , negateTrans
  , foldlAffineTransform
  , affTransSize
  , LinearConstraint
  , mkLinearConstraint
  , linConsCoeff
  , linConsConst
  , linConsSize
  , linConsAsTransform
  , foldlLinearConstraint
  , NumericalDomain(..)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric.Abstraction

-- One of the basic notions we will need for abstract interpretation is linear
-- expressions over the program variables. In general these expressions will be
-- represented by a Map from the dimension type `d` to the coefficients.

-- | An affine transformation in some space. This is a linear combination of
-- program variables plus a constant.
data AffineTransform d = AffineTransform
  { atCoeffs :: M.Map d Double
  , atConst :: Double
  }

-- | Make a new affine transform from a coefficient map and a constant. The map
-- contains coefficients for each dimension, where missing coefficients are
-- considered to be zero.
mkAffineTransform :: M.Map d Double -> Double -> AffineTransform d
mkAffineTransform m c = AffineTransform { atCoeffs = m, atConst = c }

-- | Get the coefficient of an affine transformation associated with a
-- particular dimension.
affTransCoeff :: Ord d => AffineTransform d -> d -> Maybe Double
affTransCoeff l = flip M.lookup $ atCoeffs l

-- | Get the constant of an affine transformation.
affTransConst :: AffineTransform d -> Double
affTransConst = atConst

-- | Left-associative fold over the coefficients of an affine transform.
foldlAffineTransform :: (a -> d -> Double -> a) -> a -> AffineTransform d -> a
foldlAffineTransform f i = M.foldlWithKey f i . atCoeffs

-- | Get the number of non-zero coefficients of this affine transform
affTransSize :: AffineTransform d -> Int
affTransSize = foldlAffineTransform (\n _ c -> if c == 0 then n else n + 1) 0

-- | Add one to the coefficient of a variable to a transform
addVarTrans :: Ord d => AffineTransform d -> d -> AffineTransform d
addVarTrans a v = a { atCoeffs = M.adjust (+ 1) v $ atCoeffs a }

-- | Subtract one from the coefficient of a variable from a transform
subVarTrans :: Ord d => AffineTransform d -> d -> AffineTransform d
subVarTrans a v = a { atCoeffs = M.adjust (\x -> x - 1) v $ atCoeffs a }

-- | Negate the coefficients and constant of an affine transform.
negateTrans :: Ord d => AffineTransform d -> AffineTransform d
negateTrans a = AffineTransform
  { atCoeffs = M.map (\x -> - x) $ atCoeffs a
  , atConst = - (atConst a)
  }


-- | A linear constraint over the program variables. A linear constraint
-- consists of a map from variables to coefficients and a constant. A valuation
-- of the variables satisfies the linear constraint if the linear
-- transformation defined by the coefficients is less than or equal to the
-- constant. For example, if we assume variables are indexed by integers from
-- 1 to \(n\) then the coefficients form a vector \(c \in \mathbb{R}^n\) and
-- the constant is \(b\). Then a valuation \(x \in \mathbb{R}^n\) of the
-- variable satisfies the constraint if \(c^T x \le b\).
data LinearConstraint d = LinearConstraint
  { lcCoeffs :: M.Map d Double
  , lcConst :: Double
  }
  deriving (Eq, Ord)

-- | Make a new linear constraint from a coefficient map and a constant. The
-- map contains coefficients for each dimension, where missing coefficients are
-- considered to be zero.
mkLinearConstraint :: M.Map d Double -> Double -> LinearConstraint d
mkLinearConstraint m c = LinearConstraint { lcCoeffs = m, lcConst = c }

-- | Get the coefficient of a linear constraint associated with a
-- particular dimension.
linConsCoeff :: Ord d => LinearConstraint d -> d -> Maybe Double
linConsCoeff l d = M.lookup d $ lcCoeffs l

-- | Get the constant of a linear constraint.
linConsConst :: LinearConstraint d -> Double
linConsConst = lcConst

-- | Get the number of non-zero coefficients in this constraint.
linConsSize :: LinearConstraint d -> Int
linConsSize = foldlLinearConstraint (\n _ c -> if c == 0 then n else n + 1) 0

linConsAsTransform :: LinearConstraint d -> AffineTransform d
linConsAsTransform lc = AffineTransform { atCoeffs = lcCoeffs lc
                                        , atConst = - lcConst lc }

-- | Left-associative fold over the coefficients of an affine transform
foldlLinearConstraint :: (a -> d -> Double -> a) -> a -> LinearConstraint d -> a
foldlLinearConstraint f i = M.foldlWithKey f i . lcCoeffs

-- | An abstract domain over numerical program variables. This allows the
-- abstract elements to interact with linear expressions.
class AbstractDomain d a => NumericalDomain d a | a -> d where
  -- | Transform several dimensions in parallel
  assign :: M.Map d (AffineTransform d) -> a -> a
  -- | Meet with a set of linear constraints.
  constrain :: S.Set (LinearConstraint d) -> a -> a
