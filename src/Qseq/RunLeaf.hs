{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Qseq.RunLeaf where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.Subst
import Qseq.QTypes
import Util


-- | = Running atomic queries

runVarTest :: Possible e -> sp -> Subst e -> VarTest e sp
           -> Either String Bool
runVarTest p sp s t = (varTestFunction t) sp p s

runFind :: forall e sp.
           sp -> Subst e -> Find e sp -> Either String (CondElts e)
runFind sp s (Find find deps) = do
  (found :: Set e) <- prefixLeft "runFind" $ find sp s
  let used = M.restrictKeys s deps :: Subst e
  Right $ M.fromSet (const $ S.singleton used) found

runTestOnElt :: forall e sp.
                sp -> Subst e -> Test e sp -> e
             -> Either String (Bool, Subst e)
runTestOnElt sp s (Test test deps) e = do
  (passes :: Bool) <- prefixLeft "runTestOnElt" $ test sp s e
  let used = M.restrictKeys s deps :: Subst e
  Right (passes, used)

runTest :: forall e sp. Ord e
        => sp -> Subst e -> Test e sp -> CondElts e
        -> Either String (CondElts e)
runTest sp s q ce = do
  (passed :: Map e (Bool, Subst e)) <-
    (<$>) (M.filter fst)
    $ ifLefts_map "runTest"
    $ M.mapWithKey (\k v -> runTestOnElt sp s q k) ce
  let f ::  e -> (Bool, Subst e) -> Set (Subst e)
      f k (_,s) = let ss = (M.!) ce k :: Set (Subst e)
                  in S.map (M.union s) ss
  Right $ M.mapWithKey f passed
