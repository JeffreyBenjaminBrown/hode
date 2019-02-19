{-# LANGUAGE ScopedTypeVariables #-}

module Qseq.Subst where

import           Prelude hiding (min)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes
import Util.Misc


-- | `drawVar p s src res` binds the name res to each e
-- that p indicates src can take.
drawVar :: (Ord e, Show e) =>
  Possible e -> Subst e -> Var -> Var -> Either String (Set (Subst e))
drawVar p s src res = do
  ce <- maybe (Left $ keyErr "drawVar" src p) Right
       $ M.lookup src p
  Right $ S.map (flip (M.insert res) s) $ M.keysSet ce


-- | `reconcileDetsAcrossVars p s dets` is every
-- `Subst` that permits every value in `dets` specified by `s`.
-- Each det should be bound in s.
-- The reconciliation is across dets -- that is, for each d in dets
-- and each r in the result, if s binds d to the value d0, then r is
--  consistent with at least one Subst that p assigns to the binding d=d0.
--
-- For instance, imagine a Possible p in which
-- a can be 1 provided x is 1 or 2, and
-- b can be 1 provided x is      2 or 3.
-- and we ask for `reconcileDetsAcrossVars p (a=1,b=1) (a,b)`
-- THe only Subst consistent with the bindings for both a and b is x=2.
--
-- TODO (#fast) short-circuit reconcileDetsAcrossVars.
-- varPossibilities is so far reconcileDetsAcrossVars's only caller.
-- varPossibilities only wants the Elt, not the associated Substs,
-- so if there is any way to reconcile an Elt across Substs,
-- don't search for yet more ways.

reconcileDetsAcrossVars :: forall e. (Ord e, Show e)
                        => Possible e -> Subst e -> Set Var
                        -> Either String (Set (Subst e))
reconcileDetsAcrossVars    p           s        dets
  | null dets = Left $ "reconcileDetsAcrossVars: empty 'dets' argument.\n"
  | True = do
      (se :: Set (Set (Subst e))) <- ifLefts_set "reconcileDetsAcrossVars"
                                     $ S.map (inputSubsts p s) dets
      Right $ reconcile se

-- | `inputSubsts p s v` gives all the input sets that can lead to v --
-- that is, the `Set` of `Subst`s that p associates with v=v0,
-- where v0 is the value s binds to v.

inputSubsts :: forall e. (Ord e, Show e)
              => Possible e -> Subst e -> Var -> Either String (Set (Subst e))
inputSubsts p s v = do
  vIs      <- maybe (Left $ keyErr "inputSubsts / Subst" v s)
              Right $ M.lookup v s
  vCouldBe <- maybe (Left $ keyErr "inputSubsts / Possible" v p)
              Right $ M.lookup v p
  id        $ maybe (Left $ keyErr "inputSubsts / CondElts" vIs vCouldBe)
              Right $ M.lookup vIs vCouldBe


-- | = Building a `CondElts` from `Subst`s

-- | `setSubstToCondElts v ss` is the simple union of running, across
-- all s in ss, `setSubstToCondElts v s`.

setSubstToCondElts :: forall e. Ord e
                   => Var -> Set (Subst e) -> CondElts e
setSubstToCondElts v = S.foldl f M.empty where
  f :: CondElts e -> Subst e -> CondElts e
  f ces subst = case substToCondElts v subst of
    Nothing   -> ces
    Just ces' -> M.unionWith S.union ces ces'

-- | `substToCondElts v s` constructs a `CondElts` that maps `v` to the
-- other elements of the `Subst`. This is possible iff `v`is in the `Subst`.

substToCondElts :: Var -> Subst e -> Maybe (CondElts e)
substToCondElts v subst = do
  val <- M.lookup v subst
  let subst' = M.delete v subst
  Just $ M.singleton val $ S.singleton subst'


-- | = Reconciling `CondElts`s

-- | `reconcileCondEltsAtElt ces` is the biggest `CondElts` possible
-- consistent with every `CondElt` in the input. That is, for every `Elt` e,
-- and every `Subst` s in `reconcileCondEltsAtElt ces`, and every
-- `CondElt` ce in ces, s is consistent with at least one `Subst` in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.

reconcileCondElts :: forall e. Ord e => Set (CondElts e) -> CondElts e
reconcileCondElts ces = let
   reconciled :: Set (CondElts e)
   reconciled = let f = flip reconcileCondEltsAtElt ces
                    keys = S.unions $ S.map M.keysSet ces :: Set e
     in S.map fromJust $ S.filter isJust $ S.map f keys
   in M.unions reconciled :: CondElts e

-- | `reconcileCondEltsAtElt e ces` is the biggest `CondElts` possible
-- such that each value it associates with e is consistent with each of
-- the `CondElts`s in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.
-- (Each determinant of a VarFunc implies a separate CondElts for it.)

reconcileCondEltsAtElt :: forall e. Ord e
                       => e -> Set (CondElts e) -> Maybe (CondElts e)
reconcileCondEltsAtElt e ces = do
  let maybeConds = S.map (M.lookup e) ces :: Set (Maybe (Set (Subst e)))
    -- the conditions under which `e` can obtain
  case S.member Nothing maybeConds of
    True -> Nothing
    False -> let conds = S.map fromJust maybeConds :: Set (Set (Subst e))
                 rConds = reconcile conds
      in if null rConds then Nothing
         else Just $ M.singleton e rConds


-- | = Reconciling `Subst`s

-- | `reconcile ss` finds every `Subst` that is a (maybe improper)
-- superset of at least one `Subst` from every member of ss (and contains
-- nothing not contained by one of them).
reconcile :: Ord e => Set (Set (Subst e)) -> Set (Subst e)
reconcile ss = if null ss then S.empty
               else S.foldl reconcile2sets min rest
  where (min, rest) = S.deleteFindMin ss -- like head-tail decomposition

-- | `reconcile2sets ss1 ss2` is every `Subst` that reconciles
-- something from ss1 with something from ss2.
reconcile2sets :: Ord e => Set (Subst e) -> Set (Subst e) -> Set (Subst e)
reconcile2sets ss1 ss2 = S.unions $ S.map (\s -> reconcile1ToMany s ss2) ss1

-- | In `reconcile1ToMany s ss`, there is at most one way to merge s
-- with every s' from ss. This collects them.
reconcile1ToMany :: Ord e => Subst e -> Set (Subst e) -> Set (Subst e)
reconcile1ToMany s ss = S.map fromJust $ S.filter isJust
                        $ S.map (reconcile2 s) ss

-- | If they assign different values to the same variable, it's Nothing.
-- Otherwise it's their union.
reconcile2 :: forall e. Eq e => Subst e -> Subst e -> Maybe (Subst e)
reconcile2 s t = S.foldr f (Just M.empty) allKeys where
  allKeys = S.union (M.keysSet s) (M.keysSet t) :: Set Var
  f :: Var -> Maybe (Subst e) -> Maybe (Subst e)
  f _ Nothing = Nothing -- short-circuit (hence foldr)
  f v (Just acc) =
    if        S.member v (M.keysSet s)
    then if   S.member v (M.keysSet t)
         then if (M.!) t v /= (M.!) s v
              then Nothing
              else Just $ M.insert v ((M.!) s v) acc
         else      Just $ M.insert v ((M.!) s v) acc
    else           Just $ M.insert v ((M.!) t v) acc
