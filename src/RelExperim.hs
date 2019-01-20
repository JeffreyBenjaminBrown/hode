{-# LANGUAGE ScopedTypeVariables #-}

module RelExperim where

import           Prelude
import           Data.Either
import qualified Data.List      as L
import           Data.Map (Map)
import qualified Data.Map       as M
import qualified Data.Relation  as R
import           Data.Set (Set)
import qualified Data.Set       as S

import Subst
import Types
import Util


type Rel = R.Relation
type CondElts' e = Rel e (Subst e)
type Possible' e = Map Var (CondElts' e)

data TSource = TSource {
    plans       :: [TVarPlan] -- ^ new `Var`s to introduce in this `Query`
  , haveInputs  :: [Var] -- ^ only values for which these `Var`s
    -- (as dictated by some `Subst`) are inputs will be returned
  , haveOutputs :: [(Var,Var)] -- ^ Each fst is a source (a key in the
    -- Possible), and each snd is the corresponding `Var` in the `Subst`.
  }

data TVarPlan = TVarPlan {
    tSource :: Var -- ^ where in the `Possible` values are drawn from
  , tName   :: Var -- ^ what the drawn values will be called in this `Query`
  , namesItsInputs      :: [Var]
  , isNamedByItsOutputs :: [Var]
  }

-- TODO A TSource t is valid only if
-- (1) The length of `haveInputs t`  matches the length of
--    `namesItsInputs p`     for each `p` in `plans t`.
-- (2) The length of `haveOutputs t` matches the length of
--   `isNamedByItsOutputs p` for each `p` in `plans t`.

-- | Consider `varPossibilities p s src`, where
  -- s = M.fromList [(i1,iVal), (o1,oVal)]
  -- `src = TSource vp [i1] [(o,o1)]`
  -- `vp = TVarPlan "a" "a2" ["i"] ["a1"]`.
--
-- That says:
--   a and o are the names of the results of two previous Queries.
--   When a was created, it relied on an input it called i.
--   When o was created, it relied on an input it called a1.
--   We want:
--     to draw values (call them aVal) from the results for a,
--     to bind the drawn value aVal to the name a2,
--     and to include only those values of aVal such that:
--       i=iVal was an input to the binding a=aVal.
--       a1=aVal was an input to the binding o=oVal.
--
-- We create a set of candidates for a2 by first matching inputs:
-- we find a in the `Possible`, and then restrict
-- its associated `CondElts` to include only `Subst`s that include
-- the binding i=i1v.
--
-- Next we have to match outputs. Consider a particular candidate, a2=aVal.
-- aVal meets the output-matching criteria if o uses a1=aVal as an input.
-- We find the CondElts keyed by "o" in the Possible,
-- and within that CondElts we look up oVal.
-- If any Subst in the resulting Set Subst contains the binding
-- a1=aVal, then aVal survives.

varPossibilities :: forall e. (Ord e, Show e)
                 => Possible e -> Subst e -> TSource
                 -> Either String (Possible e)
varPossibilities    p           s            t = let
  (pls, ins) = (plans t, haveInputs t)
  se, lefts :: [Either String (CondElts e)]
  se = map (f . tSource) pls where
    f :: Var -> Either String (CondElts e)
    f v = maybe (Left $ keyErr "varPossibilities" v p) Right
          $ M.lookup v p

  lefts = filter isLeft se
  in case null lefts of
  False -> Left $ "varPossibilities: error in callee:\n"
           ++ (foldr (++) "" $ map (fromLeft "") lefts)
  True -> let
    ces :: [CondElts e]
    ces = map (fromRight $ error "impossible") se

    ces_either, lefts :: [Either String (CondElts e)]
    ces_either = map f $ zip pls ces
      where f :: (TVarPlan, CondElts e) -> Either String (CondElts e)
            f (pl, ce) = restrictToMatchIns t pl s ce
    lefts = filter isLeft ces_either
    in case null lefts of
    False -> Left $ "varPossibilities: error in callee:\n"
             ++ (foldr (++) "" $ map (fromLeft "") lefts)
    True -> let
      cesRestricted = map (fromRight $ error "impossible") ces_either

      in Right $ M.fromList $ zip (map tName pls) cesRestricted
    -- TODO : match outputs also

restrictToMatchIns :: forall e. Eq e =>
  TSource -> TVarPlan -> Subst e
  -> CondElts e -> Either String (CondElts e)

restrictToMatchIns t pl s ce = let
  ins = haveInputs t
  sIns = M.restrictKeys s $ S.fromList ins :: Subst e
  s_either = M.mapKeys (renameIn t pl) sIns :: Map (Either String Var) e
  lefts = S.filter isLeft $ M.keysSet s_either :: Set (Either String Var)
  in case null lefts of
  False -> Left $ "insMatch: error in callee:\n"
           ++ (S.foldr (++) "" $ S.map (fromLeft "") lefts)

  True -> let
    sRenamed :: Subst e
    sRenamed = M.mapKeys (fromRight $ error "impossible") s_either
    supermaps = M.map ( S.filter $ M.isSubmapOf sRenamed) ce
    in Right $ M.filter (not . S.null) $ supermaps

renameIn :: TSource -> TVarPlan -> Var -> Either String Var
renameIn t pl k = let ins = haveInputs t
                      newNames = namesItsInputs pl
                      renamer = M.fromList $ zip ins newNames
  in maybe  (Left $ keyErr "renameInput" k renamer) Right
     $ M.lookup k renamer
