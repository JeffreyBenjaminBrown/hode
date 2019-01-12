{-# LANGUAGE ScopedTypeVariables #-}

module Subst where

import           Data.Either
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


extendPossible :: Var -> Possible -> Subst
  -> Either String (Possible, Set Subst)
extendPossible v p s =
  case varPossibilities p s v of
     Left s -> Left $ "extendPossible: error in callee:\n" ++ s
     Right ce -> let
       p' = if null $ varDets v then p else M.insert v ce p
         -- TODO ? Does ce need this insertion into p? It is only a subset
         -- of a the CondElts already present, keyed by (stripDets v).
       s' = S.map (\k -> M.insert v k s) $ M.keysSet ce
       in Right (p',s')

-- | `varPossibilities p s dv@(Var v dets)` is all values dv can take.
-- dv should not be bound in s or p, and every det in dets should be.
-- If dets is null, then the CondElts returned
-- has no determinants, and the Subst is not used.

varPossibilities :: Possible -> Subst -> Var -> Either String CondElts
varPossibilities    p           s        dv@(Var v dets)
  | null dets = maybe (Left $ keyErr "varPossibilities" dv p) Right
                $ M.lookup dv p
  | otherwise = let
    (pRestricted :: Possible) = M.map (f :: CondElts -> CondElts)
                                $ M.restrictKeys p vAndDets
      where -- A Subst in p that mentions a variable other than v or
            -- one of the dets is irrelevant, as is any such key of p.
        vAndDets = S.insert (stripDets dv) dets
        f = M.map $ S.map $ flip M.restrictKeys vAndDets
    in case reconcileDetsAcrossVars pRestricted s dets of
      Left s -> Left $ "varPossibilities: error in callee:\n" ++ s
      Right (substs :: Set Subst) -> let
        (possible :: Set Elt) =
          M.keysSet $ setSubstToCondElts (stripDets dv) substs
        (mce_v :: Maybe CondElts) = M.lookup (stripDets dv) p
        in maybe (Left $ keyErr "varPossibilities" dv p)
           (Right . flip M.restrictKeys possible) mce_v

stripDets :: Var -> Var
stripDets (Var name _) = Var name S.empty

-- | `reconcileDetsAcrossVars p s dets` is every
-- `Subst` that permits every value in `dets` specified by `s`.
-- Each det should be bound in s.
-- The reconciliation is across dets -- that is, for each det in dets
-- and each r in the result, r is consistent with at least one Subst
-- that p assigns to det (given the value of det in s).
--
-- For instance, imagine a Possible in which
-- a can be 1 provided x is 1 or 2, and
-- b can be 1 provided x is      2 or 3.
-- and we ask for reconcileDetsAcrossVars consistent with the subst (a=1,b=1).
-- THe only Subst consistent with both is a=2.
--
-- TODO (#fast) short-circuit reconcileDetsAcrossVars.
-- varPossibilities is so far reconcileDetsAcrossVars's only caller.
-- varPossibilities only wants the Elt, not the associated Substs,
-- so if there is any way to reconcile an Elt across Substs,
-- don't search for yet more ways.

reconcileDetsAcrossVars :: Possible -> Subst -> Set Var
                        -> Either String (Set Subst)
reconcileDetsAcrossVars    p           s        dets
  | null dets = Left $ "reconcileDetsAcrossVars: empty 'dets' argument.\n"
  | True = let
    se = S.map (impliedSubsts p s) dets :: Set (Either String (Set Subst))
    lefts = S.filter isLeft se
    in case null lefts of
      False -> Left $ S.foldr (++) "" $ S.map (fromLeft "") lefts
      True -> Right $ reconcile $ S.map (fromRight S.empty) se

-- | If s maps v to e, then `impliedSubsts p s v` returns all Substs
-- that could lead to the result v=e.

impliedSubsts :: Possible -> Subst -> Var -> Either String (Set Subst)
impliedSubsts p s v =
  case (M.lookup v s :: Maybe Elt) of
  Nothing -> Left $ keyErr "impliedSubsts / Subst" v s
  Just (vIs :: Elt) -> case M.lookup v p of
    Nothing -> Left $ keyErr "impliedSubsts / Possible" v p
    Just (vCouldHaveBeen :: CondElts) -> case M.lookup vIs vCouldHaveBeen of
      Nothing -> Left $ keyErr "impliedSubsts / CondElts" vIs vCouldHaveBeen
      Just ss -> Right ss


-- | = Building a `CondElts` from `Subst`s

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

-- | `reconcileCondEltsAtElt ces` is the biggest `CondElts` possible
-- consistent with every `CondElt` in the input. That is, for every `Elt` e,
-- and every `Subst` s in `reconcileCondEltsAtElt ces`, and every
-- `CondElt` ce in ces, s is consistent with at least one `Subst` in ces.
--
-- ASSUMES all input `CondElts` condition for `Elt` values of the same `Var`.
-- (Each determinant of a VarFunc implies a separate CondElts for it.)

reconcileCondElts :: Set CondElts -> CondElts
reconcileCondElts ces = let
   keys :: Set Elt
   keys = S.unions $ S.map M.keysSet ces
   reconciled :: Set CondElts
   reconciled = let f = flip reconcileCondEltsAtElt ces
     in S.map fromJust $ S.filter isJust $ S.map f keys
   in M.unions reconciled :: CondElts

-- | `reconcileCondEltsAtElt e ces` is the biggest `CondElts` possible
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

-- | `reconcile2sets ss1 ss2` is every `Subst` that reconciles
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
