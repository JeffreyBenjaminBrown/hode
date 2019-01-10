{-# LANGUAGE ViewPatterns #-}

module Query where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Subst
import Types
import Query.Classify


-- | = Atomic queries

runFind :: Data -> Subst -> Find -> CondElts
runFind d s (Find find deps) =
  let found = find d s             :: Set Elt
      used = M.restrictKeys s deps :: Subst
  in M.fromSet (const $ S.singleton used) found

runTestOnElt :: Data -> Subst -> Test -> Elt -> (Bool, Subst)
runTestOnElt d s (Test test deps) e =
  let passes = test d s e          :: Bool
      used = M.restrictKeys s deps :: Subst
  in (passes, used)

runTest :: Data -> Subst -> Test -> CondElts -> CondElts
runTest d s q ce = M.map harmonize passed where
  tested, passed :: Map Elt ((Bool, Subst), Set Subst)
  tested = M.mapWithKey (\k a -> (runTestOnElt d s q k, a)) ce
  passed = M.filter (fst . fst) tested
  harmonize :: ((a,Subst), Set Subst) -> Set Subst -- ignores the Bool
  harmonize ((_,s),ss) = S.map (M.union s) ss
    -- This M.union is reasonable if we have used disjointExistentials
    -- to ensure the Test does not re-assign an earlier-assigned variable.


-- | = some helpers

queryOverVarPossibilities ::
  Data -> Possible -> Query -> Subst -> Var -> Set CondElts
queryOverVarPossibilities d p q s v = S.map (runQuery d p' q) substs
 where
  vp = varPossibilities p s v :: CondElts
  p' = if null $ varDets v then p else M.insert v vp p
  substs = S.map (\k -> M.insert v k s) $ M.keysSet vp :: Set Subst

testOverVarPossibilities ::
  Data -> Possible -> Query -> Subst -> Var -> CondElts -> Set CondElts
testOverVarPossibilities d p q s v ce = ces where
  vp = varPossibilities p s v :: CondElts
  p' = if null $ varDets v then p else M.insert v vp p
  substs = S.map (\k -> M.insert v k s) $ M.keysSet vp :: Set Subst
  ces = S.map (flip (runTestable d p' q) ce) substs

runQAnd :: Data -> Possible -> [Query] -> Subst -> CondElts
runQAnd d p qs s = tested where
  (searches,tests) = partition findable qs
  found = map (flip (runQuery d p) s) searches :: [CondElts]
  reconciled, tested :: CondElts
  reconciled = reconcileCondElts $ S.fromList found
  tested = foldr (\t ce -> runTestable d p t s ce) reconciled tests


-- | `runTestable` filters the input `CondElts`.

-- TODO (#speed) runTestable: foldr with short-circuiting.
runTestable :: Data -> Possible -> Query -> Subst -> CondElts -> CondElts
runTestable _ _ (testable -> False) _ _ =
  error "runTestable: not a testable Query"
runTestable d _ (QTest t) s ce = runTest d s t ce

runTestable d p (QAnd qs) s ce = reconcileCondElts $ S.fromList results
  where results = map (\q -> runTestable d p q s ce) qs :: [CondElts]
runTestable d p (QOr qs) s ce = M.unionsWith S.union results
  where results = map (\q -> runTestable d p q s ce) qs :: [CondElts]

runTestable d p (ForSome v q) s ce = let
  ces = testOverVarPossibilities d p q s v ce
  in M.unionsWith S.union ces

runTestable d p (ForAll v q) s ce = let
  ces = testOverVarPossibilities d p q s v ce
  cesWithoutV = S.map (M.map $ S.map $ M.delete v) ces :: Set CondElts
    -- delete the dependency on v, so that reconciliation can work
  in reconcileCondElts cesWithoutV
    -- keep only results that obtain for every value of v


-- | `runQuery`

runQuery :: Data
         -> Possible -- ^ how the `Program`'s earlier `Var`s have been bound
         -> Query
         -> Subst  -- ^ earlier (higher, calling) quantifiers draw these
                   -- from the input `Possible`
         -> CondElts

runQuery _ _ (findable -> False) _ = error "runQuery: non-findable Query"
runQuery d _ (QFind f) s = runFind d s f
runQuery d p (QAnd qs) s = runQAnd d p qs s

runQuery d p (QOr qs) s =
  -- TODO (#speed|#hard) Fold QOr with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
  let ces = map (flip (runQuery d p) s) qs :: [CondElts]
  in M.unionsWith S.union ces

runQuery d p (ForSome v q) s = let
  ces = queryOverVarPossibilities d p q s v :: Set CondElts
  in M.unionsWith S.union ces

runQuery d p (ForAll v q) s = let
  -- TODO (#speed) Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.
  ces = queryOverVarPossibilities d p q s v
  cesWithoutV = S.map (M.map $ S.map $ M.delete v) ces :: Set CondElts
    -- delete the dependency on v, so that reconciliation can work
  in reconcileCondElts cesWithoutV
    -- keep only results that obtain for every value of v
