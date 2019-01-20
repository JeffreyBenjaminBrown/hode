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

data TSource = TSource { plans   :: [TVarPlan]
                       , haveInputs  :: [Var]
                       , haveOutputs :: [Var] }

data TVarPlan = TVarPlan { tSource             :: Var
                         , tName               :: Var
                         , namesItsInputs      :: [Var]
                         , isNamedByItsOutputs :: [Var] }

-- | If s binds i to i0 and o to o0, calling
-- `varPossibilities p s (TSource v (S.singleton i) (S.singleton o))`,
-- should yield all values of v for which i=i0 is an input and for which
-- o=o0 is an output.

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
