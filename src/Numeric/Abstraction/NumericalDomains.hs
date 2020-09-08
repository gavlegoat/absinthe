{-|
Module      : Numeric.Abstraction.NumericalDomains
Description : Definitions for numerical domains.
Copyright   : (c) Greg Anderson, 2020
License     : MIT
Maintainer  : ganderso@cs.utexas.edu
Stability   : experimental

Generic definitions for numerical domains and implementations of a few
numerical domains.
-}

module Numeric.Abstraction.NumericalDomains
  ( -- * Numerical Domains
    --
    -- | A numerical abstract domain constrains a space of program variables
    -- whose values come from some ordered ring (usually integers or reals).
    -- Currently only real values are supported.
    module Numeric.Abstraction.NumericalDomains.Data

    -- * Interval
    --
    -- | The interval abstract domains maintains independent bounds on each
    -- program variable.
  , module Numeric.Abstraction.NumericalDomains.Interval

  -- * Octagon
  --
  -- | The octagon abstract domain allows for constraints on pairs of program
  -- variables. Specifically, an octagon is a set of constraints of the form
  -- \(\pm x \pm y \le d\) or \(\pm x \le d\) where \(x\) and \(y\) are program
  -- variables and \(d\) is a bound. This allows for all of the constraints of
  -- intervals along with a restricted form of relational constraints.
  , module Numeric.Abstraction.NumericalDomains.Octagon
  ) where

import Numeric.Abstraction.NumericalDomains.Data
import Numeric.Abstraction.NumericalDomains.Interval
import Numeric.Abstraction.NumericalDomains.Octagon

