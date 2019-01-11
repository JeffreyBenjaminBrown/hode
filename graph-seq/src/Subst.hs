module Subst where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


extendPossible :: Var -> Possible -> Subst -> (Possible, Set Subst)
extendPossible v p s = (p',s') where
    vp = varPossibilities p s v :: CondElts
    p' = if null $ varDets v then p else M.insert v vp p
    s' = S.map (\k -> M.insert v k s) $ M.keysSet vp

-- | `varPossibilities r s (Var v dets)` returns all values v can take,
-- given r, s and v. If dets is non-null, then the CondElts returned
-- will have no dependencies. (We could include those dets as its
-- dependencies, but that would reverse the normal meaning of a Subst
-- in a Possible -- and it would only duplicate information already
-- present in the input Subst.)

varPossibilities :: Possible -> Subst -> Var -> CondElts
varPossibilities    p           s  vf@(Var v dets) = case null dets of
  True -> (M.!) p vf
  False ->
    let
      pRestricted = M.map f p
        where
  -- Any Subst in p that mentions a variable other than v or one of the
  -- dets is irrelevant. So too is any such key of p.
          vAndDets = S.insert (Var v S.empty) dets
          f = M.map $ S.map $ flip M.restrictKeys vAndDets
      substs = reconcileDepsAcrossVars pRestricted s dets :: Set Subst
      x = setSubstToCondElts (unCondition vf) substs :: CondElts
  -- `unCondition vf` because there do not yet exist conditional values of it
    in
      recordDependencies vf x

unCondition :: Var -> Var
unCondition (Var name _) = Var name S.empty

-- | `recordDependencies vf@(Var name _) ce` replaces each instance
-- of `Var name mempty` in `ce` with `vf`.
recordDependencies :: Var -> CondElts -> CondElts
recordDependencies vf ce = let
  replace :: Subst -> Subst
  replace s = let mlk = M.lookup (unCondition vf) s in case mlk of
    Nothing -> s -- TODO ? Throw an error?
    Just lk -> M.insert vf lk $ M.delete (unCondition vf) s
  in M.map (S.map replace) ce

-- | `reconcileDepsAcrossVars r s (Var _ dets)` is the set of all
-- `Subst`s that permit the values of `dets` specified by `s`.
-- They are reconciled across dets -- that is, for each det in dets
-- and each s in the result, s is consistent with at least one Subst
-- that r assigns to det (given the value of det in s).
--
-- For instance, imagine a Possible in which
-- a can be 1 provided x is 1 or 2, and
-- b can be 1 provided x is      2 or 3.
-- and we ask for reconcileDepsAcrossVars consistent with the subst (a=1,b=1).
-- THe only Subst consistent with both is a=2.
--
-- TODO (#fast) short-circuit reconcileDepsAcrossVars.
-- varPossibilities is so far reconcileDepsAcrossVars's only caller.
-- varPossibilities only wants the Elt, not the associated Substs,
-- so if there is any way to reconcile an Elt across Substs,
-- don't search for yet more ways.

reconcileDepsAcrossVars :: Possible -> Subst -> Set Var -> Set Subst
reconcileDepsAcrossVars    p           s        dets
  | null dets = error
      "Should not happen. Thrown by reconcileDepsAcrossVars. Blame varPossibilities."
  | True = let
      impliedSubsts :: Var -> Set Subst
      impliedSubsts det = (M.!) couldBindTo bound
        where
          bound       = (M.!) s det :: Elt
          couldBindTo = (M.!) p det :: CondElts
    in
      reconcile $ S.map impliedSubsts dets


-- | = Building a `CondElts` from `Subst`s
-- TODO ? Functions like `setSetSubstToCondElts` are in a sense lossy:
-- the var input disappears from the output. That might cause confusion.
-- It would be safer to use `Possible1`.

-- | `setSubstToCondElts v ss` is the simple union of running, across
-- all s in ss, `setSubstToCondElts v s`.

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

reconcileCondElts :: Set CondElts -> CondElts
reconcileCondElts ces = u where
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
  where (min, rest) = S.deleteFindMin ss -- like head-tail decomposition

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
