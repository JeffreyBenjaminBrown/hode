module Subst where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


-- | `varFuncCondVals r s (VarFunc v dets)` returns all values v can take,
-- and the `Subst`s that could lead to each, given r, s and v.
varFuncCondVals :: Result -> Subst -> VarFunc -> ConditionedElts
varFuncCondVals      r        s  vf@(VarFunc v dets) =
  case null dets of
    True -> (M.!) r v
    False -> let substs = varFuncSubsts r s vf :: Set Subst
             in restrictCondVals substs $ (M.!) r v -- TODO ! here's the bug
      -- These condvals have to be drawn not from (M.!) r v,
      -- but (somehow) from (M.!) r k for each k in dets.

-- | Each determinant implies a set of `Subst`s.
-- `varFuncSubsts` finds them, then reconciles them.
-- That is, `varFuncSubsts r s (VarFunc v dets)` is the set of all
-- `Subst`s that permit the values of `dets` determined by `s`.
--
-- (Re. names: `dets` are `Var`s that depended on `v`'s earlier calculation
-- for their own. They are bound in the `Subst`, so they determine what
-- values `v` can take.)

varFuncSubsts :: Result -> Subst -> VarFunc -> Set Subst
varFuncSubsts      r        s   (VarFunc _ dets) =
  case null dets of
    True -> error "should not happen"
    False -> let vCandidates :: Var -> Set Subst
                 vCandidates det = (M.!) couldBindTo bound where
                   bound       = (M.!) s det :: Elt
                   couldBindTo = (M.!) r det :: ConditionedElts
             in reconcile (S.map vCandidates dets)

restrictCondVals :: Set Subst -> ConditionedElts -> ConditionedElts
restrictCondVals s ces = M.unionsWith S.union
                         $ S.map (flip restrictCondVals1 ces) s

-- | The old definition, which I hope the new one is better than.
--restrictCondVals1 :: Subst -> ConditionedElts -> ConditionedElts
--restrictCondVals1 s = M.map (const $ S.singleton s)
--                      . M.filter (S.member s)

restrictCondVals1 :: Subst -> ConditionedElts -> ConditionedElts
restrictCondVals1 s = M.filter (not . S.null)
                      . M.map keepMatches where
  keepMatches :: Set Subst -> Set Subst
  keepMatches = S.filter $ isSubsetOfMap s


-- | = Reconciling `Subst`s

reconcile :: Set (Set Subst) -> Set Subst
reconcile ss = S.foldl reconcile2sets min rest where
  (min, rest) = S.deleteFindMin ss

reconcile2sets :: Set Subst -> Set Subst -> Set Subst
reconcile2sets ss1 ss2 = S.unions $ S.map (\s -> reconcile1toMany s ss2) ss1

reconcile1toMany :: Subst -> Set Subst -> Set Subst
reconcile1toMany s ss = S.map fromJust $ S.filter isJust
                $ S.map (reconcile2 s) ss

-- | If they assign different values to the same variable, it's Nothing.
-- Otherwise it's their union.
reconcile2 :: Subst -> Subst -> Maybe Subst
reconcile2 s t = S.foldl f (Just M.empty) allKeys where
  allKeys = S.union (M.keysSet s) (M.keysSet t) :: Set Var
  f :: Maybe Subst -> Var -> Maybe Subst
  f Nothing _ = Nothing -- short-circuit (roughly)
  f (Just acc) v =
    if        S.member v (M.keysSet s)
    then if   S.member v (M.keysSet t)
         then if (M.!) t v /= (M.!) s v
              then Nothing
              else Just $ M.insert v ((M.!) s v) acc
         else      Just $ M.insert v ((M.!) s v) acc
    else           Just $ M.insert v ((M.!) t v) acc
