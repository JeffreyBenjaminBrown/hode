module Subst where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


-- | `varFuncToCondElts r s (VarFunc v dets)` returns all values v can take,
-- and the relevant part** of the `Subst`s that could lead to each,
-- given r, s and v.
--
-- ** PITFALL: It isn't the whole story, just (I hope) as much as we need.
-- Specifically, the Substs in the CondElts that varFuncToCondElts
-- returns will only describe the variables that we're asking for.
--
-- For example, suppose we ask for the CondElts of x as a function of a.
-- Let xs be the possible values of x as determined by a.
-- Once we know xs, we can look up x in the Possible to find how other,
-- yet-earlier variables would have to be bound.
-- This implementation of varFuncToCondElts does not do that; it stops at x.
--
-- TODO ? does varFuncToCondElts in fact have to work farther backward?

varFuncToCondElts :: Possible -> Subst -> VarFunc -> Maybe CondElts
varFuncToCondElts      p        s  vf@(VarFunc v dets) = case null dets of
  True -> Just $ (M.!) p v
  False ->
    let -- These two steps might be duplicating a lot of work.
      substs = varFuncSubsts p s vf                        :: Set Subst
      ces = S.map (restrictCondElts substs . (M.!) p) dets :: Set CondElts
    -- TODO : missing step: "invert" the ces so that they describe v,
      -- rather than one of the dets
    in reconcileCondElts ces

-- | `varFuncSubsts r s (VarFunc _ dets)` is the set of all
-- `Subst`s that permit the values of `dets` specified by `s`.
-- They are reconciled across dets -- that is, for each det in dets
-- and each s in the result, s is consistent with the CondElts for det.

varFuncSubsts :: Possible -> Subst -> VarFunc -> Set Subst
varFuncSubsts      p           s   (VarFunc _ dets)
  | null dets = error
      "Should not happen. Thrown by varFuncSubsts. Blame varFuncToCondElts."
  | True = let impliedSubsts :: Var -> Set Subst
               impliedSubsts det = (M.!) couldBindTo bound where
                 bound       = (M.!) s det :: Elt
                 couldBindTo = (M.!) p det :: CondElts
           in reconcile $ S.map impliedSubsts dets


-- | = Using `Subst` to restrict `CondElts`

-- | `restrictCondElts ss ces` takes the simple union of the results of
--  calling `restrictCondElts ss ces` for every s in ss.
restrictCondElts :: Set Subst -> CondElts -> CondElts
restrictCondElts ss ces = M.unionsWith S.union
                         $ S.map (flip restrictCondElts1 ces) ss

-- | It is as if `restrictCondElts1 s ce` first restricts ce to those Substs
-- reconcilable with s, and then replaces each with its reconciliation.
restrictCondElts1 :: Subst -> CondElts -> CondElts
restrictCondElts1 s ce = M.filter (not . null) reconciled
  where reconciled = M.map (reconcile1ToMany s) ce


-- | = Building a `CondElts` from `Subst`s
-- TODO ? Functions like `setSetSubstToCondElts` are in a sense lossy:
-- the var input disappears from the output. That might cause confusion.
-- It would be safer to use `Possible1`.

-- | If each s in ss is a `Set Subst` derived from a different determinant**
-- of v, then `setSetSubstToCondElts v ss` creates a `CondElts` for each s,
-- and then reconciles the results.

setSetSubstToCondElts :: Var -> Set (Set Subst) -> Maybe CondElts
setSetSubstToCondElts v ss = reconcileCondElts condEltsPerDet where
  condEltsPerDet = S.map (setSubstToCondElts v) ss :: Set CondElts

-- | `setSubstToCondElts v ss` is the simple union of running, across
-- all s in ss, `setSubstToCondElts v s`.
--
-- ASSUMES that the `Subst`s all came
-- from the same `CondElts`. Hence, none of them have to be
-- reconciled. Contrast this to `setSetSubstToCondElts`, in which
-- the results from each of the innser sets must be reconciled against
-- each other, because each outer set corresponds to a separate determinant.

setSubstToCondElts :: Var -> Set Subst -> CondElts
setSubstToCondElts v = S.foldl f M.empty where
  f :: CondElts -> Subst -> CondElts
  f ces subst = case substToCondElts v subst of
    Nothing   -> ces
    Just ces' -> M.unionWith S.union ces ces'

-- | `substToCondElts v s` constructs a `CondElts` that maps `v` to the
-- other elements of the `Subst`. This is possible iff `v`is in the `Subst`.

substToCondElts :: Var -> Subst -> Maybe CondElts
substToCondElts v subst = do
  val <- M.lookup v subst
  let subst' = M.delete v subst
  return $ M.singleton val $ S.singleton subst'


-- | = Reconciling `CondElts`s

-- | `reconcileCondEltsAtElt ces`returns the biggest `CondElts` possible
-- consistent with every `CondElt` in the input. That is, for every `Elt` e,
-- and every `Subst` s in `reconcileCondEltsAtElt ces`, and every
-- `CondElt` ce in ces, s is consistent with at least one `Subst` in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.
-- (Each determinant of a VarFunc implies a separate CondElts for it.)

reconcileCondElts :: Set CondElts -> Maybe CondElts
reconcileCondElts ces = if null u then Nothing else Just u where
   keys :: Set Elt
   keys = S.unions $ S.map M.keysSet ces
   reconciled :: Set CondElts
   reconciled = let f = flip reconcileCondEltsAtElt ces
     in S.map fromJust $ S.filter isJust $ S.map f keys
   u = M.unions reconciled :: CondElts

-- | `reconcileCondEltsAtElt e ces` returns the biggest `CondElts` possible
-- such that each value it associates with e is consistent with each of
-- the `CondElts`s in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.
-- (Each determinant of a VarFunc implies a separate CondElts for it.)

reconcileCondEltsAtElt :: Elt -> Set CondElts -> Maybe CondElts
reconcileCondEltsAtElt e ces = do
  let maybeConds = S.map (M.lookup e) ces :: Set (Maybe (Set Subst))
    -- the conditions under which `e` can obtain
  case S.member Nothing maybeConds of
    True -> Nothing
    False -> let conds = S.map fromJust maybeConds :: Set (Set Subst)
                 rConds = reconcile conds
      in if null rConds then Nothing
         else Just $ M.singleton e rConds


-- | = Reconciling `Subst`s

-- | `reconcile ss` finds every `Subst` that reconciles a `Subst`
-- from every member of ss.
reconcile :: Set (Set Subst) -> Set Subst
reconcile ss = if null ss then S.empty
               else S.foldl reconcile2sets min rest
  where (min, rest) = S.deleteFindMin ss

-- | `reconcile2sets ss1 ss2` returns every `Subst` that reconciles
-- something from ss1 with something from ss2.
reconcile2sets :: Set Subst -> Set Subst -> Set Subst
reconcile2sets ss1 ss2 = S.unions $ S.map (\s -> reconcile1ToMany s ss2) ss1

-- | In `reconcile1ToMany s ss`, there is at most one way to merge s
-- with every s' from ss. This collects them.
reconcile1ToMany :: Subst -> Set Subst -> Set Subst
reconcile1ToMany s ss = S.map fromJust $ S.filter isJust
                        $ S.map (reconcile2 s) ss

-- | If they assign different values to the same variable, it's Nothing.
-- Otherwise it's their union.
reconcile2 :: Subst -> Subst -> Maybe Subst
reconcile2 s t = S.foldr f (Just M.empty) allKeys where
  allKeys = S.union (M.keysSet s) (M.keysSet t) :: Set Var
  f :: Var -> Maybe Subst -> Maybe Subst
  f _ Nothing = Nothing -- short-circuit (hence foldr)
  f v (Just acc) =
    if        S.member v (M.keysSet s)
    then if   S.member v (M.keysSet t)
         then if (M.!) t v /= (M.!) s v
              then Nothing
              else Just $ M.insert v ((M.!) s v) acc
         else      Just $ M.insert v ((M.!) s v) acc
    else           Just $ M.insert v ((M.!) t v) acc
