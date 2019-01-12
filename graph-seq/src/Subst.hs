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
extendPossible v p s = let
  e = varPossibilities p s v :: Either String CondElts
  in case e of
       Left s -> Left $ "extendPossible: error in callee:\n" ++ s
       Right ce -> let
         p' = if null $ varDets v then p else M.insert v ce p
         s' = S.map (\k -> M.insert v k s) $ M.keysSet ce
         in Right (p',s')

-- | `varPossibilities r s (Var v dets)` returns all values v can take,
-- given r, s and v. If dets is non-null, then the CondElts returned
-- will have no dependencies. (We could include those dets as its
-- dependencies, but that would reverse the normal meaning of a Subst
-- in a Possible -- and it would only duplicate information already
-- present in the input Subst.)

varPossibilities :: Possible -> Subst -> Var -> Either String CondElts
varPossibilities    p           s  vf@(Var v dets)
  | null dets = maybe (Left $ keyErr "varPossibilities" vf p) Right
                $ M.lookup vf p
  | otherwise = let
    (pRestricted :: Possible) = M.map (f :: CondElts -> CondElts)
                                $ M.restrictKeys p vAndDets
      where
       -- A Subst in p that mentions a variable other than v or
       -- one of the dets is irrelevant, as is any such key of p.
        vAndDets = S.insert (Var v S.empty) dets
        f = M.map $ S.map $ flip M.restrictKeys vAndDets
    in case reconcileDepsAcrossVars pRestricted s dets of
      Left s -> Left $ "varPossibilities: error in callee:\n" ++ s
      Right (substs :: Set Subst) -> let
        (possible :: Set Elt) =
          M.keysSet $ setSubstToCondElts (unCondition vf) substs
        (lk :: Maybe CondElts) = M.lookup (unCondition vf) p
        in maybe (Left $ keyErr "varPossibilities" vf p)
           (Right . flip M.restrictKeys possible) lk

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

reconcileDepsAcrossVars :: Possible -> Subst -> Set Var -> Either String (Set Subst)
reconcileDepsAcrossVars    p           s        dets
  | null dets = Left $ "reconcileDepsAcrossVars: empty 'dets' argument.\n"
  | True = let
    se = S.map (impliedSubsts p s) dets :: Set (Either String (Set Subst))
    lefts = S.filter isLeft se
    in case null lefts of
      False -> Left $ S.foldr (++) "" $ S.map (fromLeft "") lefts
      True -> Right $ reconcile $ S.map (fromRight S.empty) se

impliedSubsts :: Possible -> Subst -> Var -> Either String (Set Subst)
impliedSubsts p s v =
  case (M.lookup v s :: Maybe Elt) of
    Nothing -> Left $ keyErr "impliedSubsts / Subst" v s
    Just (bound :: Elt) -> case M.lookup v p of
      Nothing -> Left $ keyErr "impliedSubsts / Possible" v p
      Just (couldBindTo :: CondElts) -> case M.lookup bound couldBindTo of
        Nothing -> Left $ keyErr "impliedSubsts / CondElts" bound couldBindTo
        Just ss -> Right ss


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
