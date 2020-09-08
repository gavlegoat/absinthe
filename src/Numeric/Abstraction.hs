{-|
Module      : Numeric.Abstraction
Description : The base classes for abstract interpretation
Copyright   : (c) Greg Anderson, 2020
License     : MIT
Maintainer  : ganderso@cs.utexas.edu
Stability   : experimental

This module contains basic classes needed for abstract interpretation, along
with several generic operations.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Numeric.Abstraction
  (
  -- * Domains
  --
  -- $domains
    AbstractDomain(..)
  -- * Folds
  --
  -- $folds
  , joins
  , meets
  ) where

import qualified Data.Set as S
import Data.Foldable

-- $domains
--
-- Fundamentally, abstractions constrain some space of program varaibles.  We
-- don't want to force any particular interpretation of the dimensions, so we
-- will parameterize most data structures over a type variable (usually @d@)
-- which represents some label for the variables. For example, the user could
-- use the names of the program variables in which case @d@ would be @String@.

-- | An 'AbstractDomain' captures the basic operations required for abstract
-- interpretation.
class AbstractDomain d a | a -> d where
  -- | The 'top' element contains every value.
  top :: S.Set d -> a
  -- | The 'bottom' element contains no values.
  bottom :: S.Set d -> a
  -- | The 'join' is the least upper bound of two elements.
  join :: a -> a -> a
  -- | The 'meet' is the greatest lower bound of two elements.
  meet :: a -> a -> a
  -- | Determine whether a given element is top.
  isTop :: a -> Bool
  -- | Determine whether a given element is bottom.
  isBottom :: a -> Bool
  -- | Remove dimensions from the constrained space.
  removeVars :: S.Set d -> a -> a
  -- | Get the set of dimensions constrained by this element.
  constrainedVars :: a -> S.Set d

-- $folds
--
-- These functions operate on sets of abstract elements at a time. Each takes
-- in initial set of dimensions to be used if the set of elements is empty.
-- In general the set of dimensions in the returned abstraction will be the
-- union of the constraine dimensions in each abstract element and the initial
-- provided set of dimensions.

-- | Find the least upper bound of several elements of an abstract domain.
-- This function assumes it's argument is not empty.
joins :: (AbstractDomain d a, Foldable f) => S.Set d -> f a -> a
joins s = foldl' join $ bottom s

-- | Find the greatest lower bound of several elements of an abstract domain.
-- This function assumes it's argument is not empty.
meets :: (AbstractDomain d a, Foldable f) => S.Set d -> f a -> a
meets s = foldl' meet $ top s

