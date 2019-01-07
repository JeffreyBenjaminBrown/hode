module Subst where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


substToCondElts :: Var -> Subst -> Maybe ConditionedElts
substToCondElts v subst = do
  val <- M.lookup v subst
  let subst' = M.delete v subst
  return $ M.singleton val $ S.singleton subst'

setSubstToCondElts :: Var -> Set Subst -> Maybe ConditionedElts
setSubstToCondElts v = S.foldr f $ Just M.empty where
  f :: Subst -> Maybe ConditionedElts -> Maybe ConditionedElts
  f _ Nothing = Nothing -- short-circuit (hence foldr)
  f subst (Just ces) = case substToCondElts v subst of
    Nothing -> Nothing
    Just ces' -> Just $ M.unionWith S.union ces ces'

-- | `varFuncToCondVals r s (VarFunc v dets)` returns all values v can take,
-- and the `Subst`s that could lead to each, given r, s and v.
varFuncToCondVals :: Result -> Subst -> VarFunc -> ConditionedElts
varFuncToCondVals      r        s  vf@(VarFunc v dets) = case null dets of
  True -> (M.!) r v
  False -> let
    substs = varFuncSubsts r s vf :: Set Subst
    ces = S.map (restrictCondVals substs . (M.!) r) dets
      :: Set ConditionedElts -- each member should be a singleton set
    sss = S.map (snd . M.findMin) ces :: Set (Set Subst)
    ss = reconcile sss
    -- last step: from some substs including x,
    -- create a ConditionedElts for x
    in M.empty -- TODO finish

-- Could test to be sure those ConditionedElts in ces are all singleton maps
--varFuncToCondVals' :: Result -> Subst -> VarFunc -> Bool
--varFuncToCondVals'      r        s  vf@(VarFunc v dets) = case null dets of
--  True -> (M.!) r v
--  False -> let
--    substs = varFuncSubsts r s vf :: Set Subst
--    ces = S.map (restrictCondVals substs . (M.!) r) dets
--      :: Set ConditionedElts
--    is
--    -- in S.null $ S.filter (not . (==) 1 . S.size) ces

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
