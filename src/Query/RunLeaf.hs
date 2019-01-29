{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Query.RunLeaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Query.Valid
import Subst
import Types
import Util


-- | = Running atomic queries

runVarTest :: Possible e -> sp -> Subst e -> VarTest e sp -> Bool
runVarTest p sp s t = (varTestFunction t) p sp s

runFind :: forall e sp.
           sp -> Subst e -> Find e sp -> CondElts e
runFind sp s (Find find deps) =
  M.fromSet (const $ S.singleton used) found
  where
    found = find sp s             :: Set e
    used = M.restrictKeys s deps :: Subst e

runTestOnElt :: forall e sp.
                sp -> Subst e -> Test e sp -> e -> (Bool, Subst e)
runTestOnElt sp s (Test test deps) e = (passes, used)
  where
    passes = test sp s e          :: Bool
    used = M.restrictKeys s deps :: Subst e

runTest :: forall e sp. Ord e
        => sp -> Subst e -> Test e sp -> CondElts e -> CondElts e
runTest sp s q ce = let
  passed :: Map e (Bool, Subst e)
  passed = M.filter fst tested where
    tested = M.mapWithKey (\k v -> runTestOnElt sp s q k) ce
  f ::  e -> (Bool, Subst e) -> Set (Subst e)
  f k (_,s) = let ss = (M.!) ce k :: Set (Subst e)
              in S.map (M.union s) ss
  in M.mapWithKey f passed
