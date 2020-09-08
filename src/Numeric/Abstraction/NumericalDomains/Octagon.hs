{-|
Module      : Numeric.Abstraction.NumericalDomains.Octagon
Description : Definitions for numerical domains.
Copyright   : (c) Greg Anderson, 2020
License     : MIT
Maintainer  : ganderso@cs.utexas.edu
Stability   : experimental

Implementation of the octagon domain. This implementation closely follows the
presentation in "The Octagon Abstract Domain" by Antoine Mine,
https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numeric.Abstraction.NumericalDomains.Octagon
  ( Octagon
  ) where

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Data.Semigroup
import Data.List (intercalate)
import Data.Maybe (fromJust)

import Numeric.Abstraction
import Numeric.Abstraction.NumericalDomains.Data

-- An octagon consists of constraints of the form \pm v_i \pm v_j <= c. We
-- represent this using a a map from pairs variables labeled as positive or
-- negative to real-valued bounds. Pairs missing from the map are assumed to
-- be unbounded. To allow for constraints of the form \pm v_i <= c, we allow
-- a special "absent" variable which is always zero. Then \pm v_i <= c can be
-- represented as \pm v_i + absent <= c.

-- | A label for variables to appear positive or negative, and to allow the
-- special absent variable. Note that the Ord instance here is just to allow
-- mapping and has no semantic significance.
data OctVar d = NegVar d | Absent | PosVar d
  deriving (Eq, Ord)

instance Show d => Show (OctVar d) where
  show Absent = ""
  show (NegVar a) = "-" ++ show a
  show (PosVar a) = "+" ++ show a

-- Given a variable in an octagon bound, extract the program variable it affects.
baseVar :: OctVar d -> Maybe d
baseVar Absent     = Nothing
baseVar (PosVar x) = Just x
baseVar (NegVar x) = Just x

-- Switch negative variables to positive and vice versa.
swapVar :: OctVar d -> OctVar d
swapVar (PosVar x) = NegVar x
swapVar (NegVar x) = PosVar x
swapVar Absent     = Absent

-- A half bound can be a real number or infinity.
data HalfBound = Finite Double | Infinite
  deriving (Eq, Ord)

instance Show HalfBound where
  show (Finite a) = show a
  show Infinite   = "oo"

-- The semigroup behavior on half bounds is normal addition. There is no
-- additive inverse for infinity, so we just have semigroups.
instance Semigroup HalfBound where
  (Finite a) <> (Finite b) = Finite $ a + b
  _          <> _          = Infinite

instance Monoid HalfBound where
  mempty = Finite 0.0
  mappend = (<>)

halveBound :: HalfBound -> HalfBound
halveBound (Finite a) = Finite $ a / 2
halveBound Infinite   = Infinite

doubleBound :: HalfBound -> HalfBound
doubleBound (Finite a) = Finite $ 2 * a
doubleBound Infinite   = Infinite

-- | An octagon consists of a number of constraints between pairs of variables.
data Octagon d = OctBottom (S.Set d)
               | Octagon (M.Map (OctVar d, OctVar d) HalfBound)

instance Show d => Show (Octagon d) where
  show (OctBottom s) = "Bottom: (" ++ show s ++ ")"
  show (Octagon m)   = intercalate "\n" $ M.elems $ M.mapWithKey showBound m
    where
      showBound (i, j) b = show i ++ " " ++ show (swapVar j) ++
                                " <= " ++ show b

octTop :: Ord d => S.Set d -> Octagon d
octTop s = Octagon $ S.foldl' (\m a -> M.insert a Infinite m) M.empty $
  S.cartesianProduct (S.insert Absent $ S.map NegVar s)
                     (S.insert Absent $ S.map PosVar s)

octBottom :: S.Set d -> Octagon d
octBottom = OctBottom

octJoin :: Ord d => Octagon d -> Octagon d -> Octagon d
octJoin (OctBottom _) o             = o
octJoin o             (OctBottom _) = o
octJoin o1 o2 = case closure o1 of
                  OctBottom s -> OctBottom s
                  Octagon a -> case closure o2 of
                                 OctBottom s -> OctBottom s
                                 Octagon b -> Octagon $ M.unionWith max a b

octMeet :: Ord d => Octagon d -> Octagon d -> Octagon d
octMeet (OctBottom s)   _           = OctBottom s
octMeet _             (OctBottom s) = OctBottom s
octMeet (Octagon a)   (Octagon b)   = Octagon $ M.unionWith min a b

octIsTop :: Ord d => Octagon d -> Bool
octIsTop o = case closure o of
               OctBottom _ -> False
               Octagon m   -> M.null $ M.filter isFinite m where
                 isFinite (Finite _) = True
                 isFinite Infinite   = False

octIsBottom :: Ord d => Octagon d -> Bool
octIsBottom (OctBottom _) = True
octIsBottom o             = case closure o of
                              OctBottom _ -> True
                              Octagon _   -> False

octRemoveVars :: Ord d => S.Set d -> Octagon d -> Octagon d
octRemoveVars s (OctBottom s') = OctBottom $ S.difference s' s
octRemoveVars s (Octagon m) = Octagon $ M.filterWithKey (checkKey s) m where
  checkKey st (i, j) _ = case (baseVar i, baseVar j) of
                           (Nothing, Nothing) -> True
                           (Just i', Nothing) -> not $ S.member i' st
                           (Nothing, Just j') -> not $ S.member j' st
                           (Just i', Just j') ->
                             not $ S.member i' st || S.member j' st

octConstrainedVars :: Ord d => Octagon d -> S.Set d
octConstrainedVars (OctBottom s) = s
octConstrainedVars (Octagon m)   = M.foldlWithKey addVars S.empty m where
  addVars s (i, j) _ = (case baseVar i of
                          Nothing -> id
                          Just x -> S.insert x) $ case baseVar j of
                                                    Nothing -> s
                                                    Just y -> S.insert y s

-- Evaluate an affine transform over an octagon using interval arithmatic. The
-- return is a pair of half bounds where Infinite in the first bound indicates
-- an interval which is unbounded below. This just forgets relational
-- constraints.
intEval :: Ord d => Octagon d -> AffineTransform d -> (HalfBound, HalfBound)
intEval (OctBottom _) _ = (Finite 1, Finite 0)
intEval (Octagon m)   t = foldlAffineTransform (\(l, u) v c ->
  let lo = case halveBound $ m ! (PosVar v, NegVar v) of
             Finite a -> Finite $ - a
             Infinite -> Infinite
      hi = halveBound $ m ! (NegVar v, PosVar v)
   in if c == 0
         then (l, u)
         else case (lo, hi) of
                (Infinite,  Infinite ) -> (Infinite, Infinite)
                (Infinite,  Finite u') -> if c < 0
                                             then (Finite $ c * u', Infinite)
                                             else (Infinite, Finite $ c * u')
                (Finite l', Infinite ) -> if c < 0
                                             then (Infinite, Finite $ c * l')
                                             else (Finite $ c * l', Infinite)
                (Finite l', Finite u') ->
                  if c < 0
                     then (Finite $ c * u', Finite $ c * l')
                     else (Finite $ c * l', Finite $ c * u'))
   (Finite $ affTransConst t, Finite $ affTransConst t) t

octAssign :: Ord d => M.Map d (AffineTransform d) -> Octagon d -> Octagon d
octAssign assigns oct = assign' assigns $ closure oct where
  assign' _  o@(OctBottom _) = o
  assign' as (Octagon m)     = Octagon $ M.foldlWithKey assignOne m as
  assignOne mp j0 t
    | affTransSize t == 0                  = exact0 j0 mp t
    | affTransSize t == 1 && affCoeffOne t = exact1 j0 mp t
    | otherwise                            = rel j0 mp t
  affCoeffOne = foldlAffineTransform (\b _ c -> b && (c == 1 || c == -1)) True
  -- Assigning a constant
  exact0 j0 mp t = M.mapWithKey (\(i, j) x ->
                                   if i == PosVar j0 && j == NegVar j0
                                      then Finite $ -2 * affTransConst t
                                      else if i == NegVar j0 && j == PosVar j0
                                              then Finite $ 2 * affTransConst t
                                              else x) mp
  -- Assignment of the form X = +/- Y +/- c for constant c
  exact1 j0 mp t =
    let (i0, c) = foldlAffineTransform (\_ x y -> (x, y)) (j0, 0) t
     in if i0 == j0
           then if c == 1
                   then assignAdd j0 mp $ affTransConst t
                   else if affTransConst t == 0.0
                           then negate1 j0 mp
                           else assignAdd j0 (negate1 j0 mp) $ affTransConst t
           else if c == 1
                   then assignAdd2 j0 i0 mp $ affTransConst t
                   else if affTransConst t == 0.0
                           then negate2 j0 i0 mp
                           else assignAdd j0 (negate2 j0 i0 mp) $ affTransConst t
  assignAdd j0 mp a =
    M.mapWithKey (\(i, j) x ->
      case (i, j) of
        (PosVar i', PosVar j') | i' == j0 && j' /= j0 -> x <> Finite (- a)
                               | i' /= j0 && j' == j0 -> x <> Finite a
                               | otherwise -> x
        (NegVar i', PosVar j')
          | (i' == j0 && j' /= j0) || (i' /= j0 && j' == j0) -> x <> Finite a
          | i' == j0 && j' == j0 -> x <> Finite (2 * a)
          | otherwise -> x
        (PosVar i', NegVar j')
          | (i' == j0 && j' /= j0) || (i' /= j0 && j' == j0) ->
            x <> Finite (- a)
          | i' == j0 && j' == j0 -> x <> Finite (-2 * a)
          | otherwise -> x
        (NegVar i', NegVar j') | i' /= j0 && j' == j0 -> x <> Finite (- a)
                               | i' == j0 && j' /= j0 -> x <> Finite a
                               | otherwise -> x
        _ -> x) mp
  assignAdd2 j0 i0 mp a =
    M.mapWithKey (\(i, j) x ->
      case (i, j) of
        (PosVar i', PosVar j') | i' == j0 && j' == i0 -> Finite $ - a
                               | i' == i0 && j' == j0 -> Finite a
                               | otherwise            -> x
        (NegVar i', NegVar j') | i' == i0 && j' == j0 -> Finite $ - a
                               | i' == j0 && j' == i0 -> Finite a
                               | otherwise            -> x
        _ -> x) mp
  negate1 j0 mp =
    M.mapWithKey (\(i, j) x ->
      case baseVar i of
        Nothing -> x
        Just i' -> case baseVar j of
                     Nothing -> x
                     Just j' | i' == j0 && j' /= j0 -> mp ! (swapVar i, j)
                             | i' /= j0 && j' == j0 -> mp ! (i, swapVar j)
                             | i' == j0 && j' == j0 ->
                               mp ! (swapVar i, swapVar j)
                             | otherwise -> x) mp
  negate2 j0 i0 mp = negate1 j0 $ assignAdd2 j0 i0 mp 0.0
  -- All oth negateTrans t
  rel j0 mp t = M.mapWithKey (\(i, j) x ->
    if i == NegVar j0 && j == PosVar j0
       then doubleBound . snd $ intEval (Octagon mp) t
    else if i == PosVar j0 && j == NegVar j0
       then case fst $ intEval (Octagon mp) t of
              Finite a -> Finite $ -2 * a
              Infinite -> Infinite
    else if j == PosVar j0
       then case i of
              PosVar i0 -> if i0 == j0
                              then x
                              else snd $ intEval (Octagon mp) $ subVarTrans t i0
              Absent -> snd $ intEval (Octagon mp) t
              NegVar i0 -> snd $ intEval (Octagon mp) $ addVarTrans t i0
    else if j == NegVar j0
       then case i of
              PosVar i0 -> snd $ intEval (Octagon mp) $
                negateTrans $ addVarTrans t i0
              Absent -> snd $ intEval (Octagon mp) $ negateTrans t
              NegVar i0 -> if i0 == j0
                              then x
                              else snd $ intEval (Octagon mp) $
                                addVarTrans (negateTrans t) i0
    else if i == PosVar j0
       then case j of
              PosVar i0 -> if i0 == j0
                              then x
                              else snd $ intEval (Octagon mp) $
                                addVarTrans (negateTrans t) i0
              Absent -> snd $ intEval (Octagon mp) $  negateTrans t
              NegVar i0 -> snd $ intEval (Octagon mp) $ negateTrans $
                addVarTrans t i0
    else if i == NegVar j0
       then case j of
              PosVar i0 -> snd $ intEval (Octagon mp) $ addVarTrans t i0
              Absent -> snd $ intEval (Octagon mp) t
              NegVar i0 -> if i0 == j0 then x
                                       else snd $ intEval (Octagon mp) $
                                         subVarTrans t i0
    else x) mp

octConstrain :: (Ord d) => S.Set (LinearConstraint d) -> Octagon d -> Octagon d
octConstrain lcs oct = let oct' = closure oct
                        in meets (octConstrainedVars oct) $
                          map (oneConstraint oct') $ S.elems lcs
 where
  oneConstraint o@(OctBottom _) _ = o
  oneConstraint o@(Octagon m) lc =
    -- First we look for cases which can be handled by the exact tests
    let n = linConsSize lc
     in if n == 0
           then if linConsConst lc <= 0
                   then Octagon m
                   else OctBottom $ octConstrainedVars o
        else if n == 1
           then let j0 = fromJust $ foldlLinearConstraint
                           (\a d c -> if c == 0 then a else Just d)
                           Nothing lc
                 in if fromJust (linConsCoeff lc j0) == 1
                       then Octagon $ M.adjust
                         (min (Finite $ 2 * linConsConst lc))
                         (NegVar j0, PosVar j0) m
                    else if fromJust (linConsCoeff lc j0) == -1
                       then Octagon $ M.adjust
                         (min (Finite $ 2 * linConsConst lc))
                         (PosVar j0, NegVar j0) m
                    else Octagon $ M.unionWith min (modify m lc) m
        else if n == 2
           then let (i0m, j0m) =  -- Extract the vars with non-zero coeffs.
                      foldlLinearConstraint
                        (\a d c -> if c == 0
                                      then a
                                      else case fst a of
                                             Nothing -> (Just d, Nothing)
                                             Just t -> (Just t, Just d))
                        (Nothing, Nothing) lc
                    (i0, j0) = (fromJust i0m, fromJust j0m)
                 in if fromJust (linConsCoeff lc i0) == 1 &&
                         fromJust (linConsCoeff lc j0) == 1
                       then Octagon $ M.adjust
                         (min (Finite $ linConsConst lc))
                         (NegVar i0, PosVar j0) $ M.adjust
                         (min (Finite $ linConsConst lc))
                         (NegVar j0, PosVar i0) m
                   else if fromJust (linConsCoeff lc i0) == 1 &&
                           fromJust (linConsCoeff lc j0) == -1
                      then Octagon $ M.adjust
                        (min (Finite $ linConsConst lc))
                        (PosVar j0, PosVar i0) $ M.adjust
                        (min (Finite $ linConsConst lc))
                        (NegVar i0, NegVar j0) m
                   else if fromJust (linConsCoeff lc i0) == -1 &&
                           fromJust (linConsCoeff lc j0) == 1
                      then Octagon $ M.adjust
                        (min (Finite $ linConsConst lc))
                        (PosVar i0, PosVar j0) $ M.adjust
                        (min (Finite $ linConsConst lc))
                        (NegVar j0, NegVar i0) m
                   else if fromJust (linConsCoeff lc i0) == -1 &&
                           fromJust (linConsCoeff lc j0) == -1
                      then Octagon $ M.adjust
                        (min (Finite $ linConsConst lc))
                        (PosVar i0, NegVar j0) $ M.adjust
                        (min (Finite $ linConsConst lc))
                        (PosVar j0, NegVar i0) m
                   else Octagon $ M.unionWith min (modify m lc) m
        else Octagon $ M.unionWith min (modify m lc) m
  -- All other cases are handled by modify
  modify mp lc = M.mapWithKey (modifyElem mp lc) mp
  modifyElem mp' lc (i, j) x =
    case (i, j) of
      (NegVar i0, PosVar j0)
        | j0 == i0 -> doubleBound . snd $ intEval (Octagon mp') $
                      addVarTrans (negateTrans $ linConsAsTransform lc) j0
        | otherwise -> snd $ intEval (Octagon mp') $ addVarTrans
                       (addVarTrans (negateTrans $ linConsAsTransform lc) i0) j0
      (PosVar i0, NegVar j0)
        | j0 == i0 -> case fst $ intEval (Octagon mp') $ negateTrans $
                           addVarTrans (linConsAsTransform lc) j0 of
                        Finite a -> Finite $ -2 * a
                        Infinite -> Infinite
        | otherwise -> snd $ intEval (Octagon mp') $ negateTrans $
                       addVarTrans (addVarTrans (linConsAsTransform lc) j0) i0
      (PosVar i0, PosVar j0)
        | j0 == i0 -> x
        | otherwise -> snd $ intEval (Octagon mp') $ addVarTrans (negateTrans $
                       addVarTrans (linConsAsTransform lc) i0) j0
      (NegVar i0, NegVar j0)
        | j0 == i0 -> x
        | otherwise -> snd $ intEval (Octagon mp') $ addVarTrans (negateTrans $
                       addVarTrans (linConsAsTransform lc) i0) j0
      _ -> x

-- Reduce an octagon to a canonical representation.
closure :: Ord d => Octagon d -> Octagon d
closure o@(OctBottom _) = o
closure o@(Octagon m) =
  let vs = octConstrainedVars o
      nm = S.foldl' updateVar m vs
   in S.foldl' (checkBound vs) (Octagon nm) vs where
     updateVar mp v = M.mapWithKey (normalize mp) $
       M.mapWithKey (updateIndex mp v) mp
     updateIndex m' k (i, j) x =
       minimum
         [ x
         , m' ! (i, PosVar k) <> m' ! (PosVar k, j)
         , m' ! (i, NegVar k) <> m' ! (NegVar k, j)
         , m' ! (i, PosVar k) <> m' ! (PosVar k, NegVar k) <> m' ! (NegVar k, j)
         , m' ! (i, NegVar k) <> m' ! (NegVar k, PosVar k) <> m' ! (PosVar k, j)
         ]
     normalize m' (i, j) x =
       min x (halveBound $ m' ! (i, swapVar i) <> m' ! (swapVar j, j))
     checkBound _ oc@(OctBottom _) _ = oc
     checkBound s (Octagon mp)    v =
       if (mp ! (PosVar v, PosVar v) < Finite 0.0) ||
            (mp ! (NegVar v, NegVar v) < Finite 0.0)
          then OctBottom s
          else Octagon $ M.adjust (const $ Finite 0.0) (PosVar v, PosVar v) $
                         M.adjust (const $ Finite 0.0) (NegVar v, NegVar v) mp

instance Ord d => Eq (Octagon d) where
  a == b = case (closure a, closure b) of
             (OctBottom s1, OctBottom s2) -> s1 == s2
             (Octagon m1  , Octagon m2  ) -> m1 == m2
             _                            -> False

instance Ord d => AbstractDomain d (Octagon d) where
  top = octTop
  bottom = octBottom
  join = octJoin
  meet = octMeet
  isTop = octIsTop
  isBottom = octIsBottom
  removeVars = octRemoveVars
  constrainedVars = octConstrainedVars

instance Ord d => NumericalDomain d (Octagon d) where
  assign = octAssign
  constrain = octConstrain
