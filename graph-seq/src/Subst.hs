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
varFuncToCondElts    p           s  vf@(VarFunc v dets) = case null dets of
  True -> Just $ (M.!) p v
  False -> let substs = varFuncSubsts p s vf :: Set Subst
               implied = setSubstToCondElts v substs :: CondElts
           in if null implied then Nothing else Just implied

-- | `varFuncSubsts r s (VarFunc _ dets)` is the set of all
-- `Subst`s that permit the values of `dets` specified by `s`.
-- They are reconciled across dets -- that is, for each det in dets
-- and each s in the result, s is consistent with the CondElts for det.

varFuncSubsts :: Possible -> Subst -> VarFunc         -> Set Subst
varFuncSubsts    p           s       (VarFunc _ dets)
  | null dets = error
      "Should not happen. Thrown by varFuncSubsts. Blame varFuncToCondElts."
  | True = let impliedSubsts :: Var -> Set Subst
               impliedSubsts det = (M.!) couldBindTo bound where
                 bound       = (M.!) s det :: Elt
                 couldBindTo = (M.!) p det :: CondElts
           in reconcile $ S.map impliedSubsts dets


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
