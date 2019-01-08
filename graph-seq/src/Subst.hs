module Subst where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


-- | `varFuncToCondVals r s (VarFunc v dets)` returns all values v can take,
-- and the `Subst`s that could lead to each, given r, s and v.
varFuncToCondVals :: Result -> Subst -> VarFunc -> CondElts
varFuncToCondVals      r        s  vf@(VarFunc v dets) = case null dets of
  True -> (M.!) r v
  False -> let
    substs = varFuncSubsts r s vf :: Set Subst
    ces = S.map (restrictCondVals substs . (M.!) r) dets
      :: Set CondElts -- each member should be a singleton set
    sss = S.map (snd . M.findMin) ces :: Set (Set Subst)
    ss = reconcile sss
    -- last step: from some substs including x,
    -- create a CondElts for x
    in M.empty -- TODO finish

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
                   couldBindTo = (M.!) r det :: CondElts
             in reconcile (S.map vCandidates dets)


-- | = Using `Subst` to restrict `CondElts`

restrictCondVals :: Set Subst -> CondElts -> CondElts
restrictCondVals s ces = M.unionsWith S.union
                         $ S.map (flip restrictCondVals1 ces) s

restrictCondVals1 :: Subst -> CondElts -> CondElts
restrictCondVals1 s = M.filter (not . S.null)
                      . M.map keepMatches where
  keepMatches :: Set Subst -> Set Subst
  keepMatches = S.filter $ isSubsetOfMap s


-- | = Building a `CondElts` from `Subst`s

-- | `setSetSubstToCondElts` presumes that each `Set Subst` came from the
-- `CondElts` correspoding to a separate determinant** of `v`.
-- Since each of those determinants
-- is presumably determined, the results of applying `setSubstToCondElts`
-- to each of those determinants must be reconciled across determinants.
--
-- ** For instance, if `v` depends on two determinants, then the
-- `Set (Set Subst)` has two members.

setSetSubstToCondElts :: Var -> Set (Set Subst) -> Maybe CondElts
setSetSubstToCondElts v ss = reconcileCondElts condEltsPerDet where
  condEltsPerDet = S.map (setSubstToCondElts v) ss :: Set CondElts

-- | `setSubstToCondElts v ss` presumes that the `Subst`s all came
-- from the same `CondElts`. Hence, none of them have to be
-- reconciled. Contrast this to `setSetSubstToCondElts`, in which
-- the results from each of the innser sets must be reconciled against
-- each other.
setSubstToCondElts :: Var -> Set Subst -> CondElts
setSubstToCondElts v = S.foldl f M.empty where
  f :: CondElts -> Subst -> CondElts
  f ces subst = case substToCondElts v subst of
    Nothing   -> ces
    Just ces' -> M.unionWith S.union ces ces'

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
-- ASSUMES all input `CondElts`
-- condition for `Elt` values of the same `Var`.
reconcileCondElts :: Set CondElts -> Maybe CondElts
reconcileCondElts ces = if null u then Nothing else Just u where
   keys = S.unions $ S.map M.keysSet ces :: Set Elt
   recds = S.map fromJust $ S.filter isJust $ S.map f keys :: Set CondElts
     where f = flip reconcileCondEltsAtElt ces
   u = M.unions recds

-- | `reconcileCondEltsAtElt e ces` returns the biggest `CondElts` possible
-- such that each value it associates with e is consistent with each of
-- the `CondElts`s in ces.
--
-- ASSUMES all input `CondElts`
-- condition for `Elt` values of the same `Var`.
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
reconcile2sets ss1 ss2 = S.unions $ S.map (\s -> reconcile1toMany s ss2) ss1

-- | In `reconcile1toMany s1 ss`, there is at most one way to merge s1
-- with every s from ss. This collects them.
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
