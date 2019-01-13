{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Query where

import           Data.Either
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

runVarTest :: Data -> Subst -> VarTest -> Bool
runVarTest d s t = (varTestFunction t) d s

runFind :: Data -> Subst -> Find -> CondElts
runFind d s (Find find deps) =
  M.fromSet (const $ S.singleton used) found
  where
    found = find d s             :: Set Elt
    used = M.restrictKeys s deps :: Subst

runTestOnElt :: Data -> Subst -> Test -> Elt -> (Bool, Subst)
runTestOnElt d s (Test test deps) e = (passes, used)
  where
    passes = test d s e          :: Bool
    used = M.restrictKeys s deps :: Subst

runTest :: Data -> Subst -> Test -> CondElts -> CondElts
runTest d s q ce = let
  passed :: Map Elt (Bool, Subst)
  passed = M.filter fst tested where
    tested = M.mapWithKey (\k v -> runTestOnElt d s q k) ce
  f ::  Elt -> (Bool, Subst) -> Set Subst
  f k (_,s) = let ss = (M.!) ce k :: Set Subst
              in S.map (M.union s) ss
  in M.mapWithKey f passed


-- | == Complex queries

-- | = runQAnd
-- There are three types of queries: testlike, findlike, and VarTests.
-- varTests and testlike queries can only be run from a conjunction (QAnd).
-- A conjunction runs all varTests first, because they are simplest: they
-- don't require reference to the Possible, and often not the data either.
-- If all Vars in the Subst pass all VarTests, then the findlike queries are
-- run. Once those results are collected, they are filtered by the testlike
-- queries.

runQAnd :: Data -> Possible -> [Query] -> Subst -> Either String CondElts
runQAnd d p qs s = let
  errMsg = "runQAnd: error in callee:\n"
  (searches,tests') = partition findlike qs
  (varTests,tests) = partition (\case QVarTest _->True; _->False) tests'
  varTestResults = map (runVarTest d s . unwrap) varTests where
    unwrap = \case QVarTest t->t; _->error "runQAnd: unwrap: impossible."
  in if not $ and varTestResults then Right M.empty else let

    eFound :: [Either String CondElts]
    eFound = map (flip (runFindlike d p) s) searches
    lefts = filter isLeft eFound
    in case null lefts of
    False -> Left $ errMsg ++ show (map (fromLeft "") lefts)
    True -> let
      (found :: [CondElts]) = map (fromRight M.empty) eFound
      (reconciled :: CondElts) = reconcileCondElts $ S.fromList found
      tested = foldr f (Right reconciled) tests where
        f :: Query -> Either String CondElts -> Either String CondElts
        f _ (Left s) = Left $ errMsg ++ s -- collect no further errors
        f t (Right ce) = runTestlike d p t s ce
      in tested


-- | = runTestlike

-- TODO (#fast) runTestlike: foldr with short-circuiting.
runTestlike :: Data -> Possible -> Query -> Subst -> CondElts
            -> Either String CondElts
runTestlike _ _ (testlike -> False) _ _ =
  Left $ "runTestlike: not a testlike Query"
runTestlike d _ (QTest t) s ce = Right $ runTest d s t ce
runTestlike _ _ (QVarTest _) _ _ =
  Left $ "runTestlike: VarTest should have been handled by QAnd."

runTestlike d p (QAnd qs) s ce = let
  results = map (\q -> runTestlike d p q s ce) qs :: [Either String CondElts]
  lefts = filter isLeft results
  in case null lefts of
  False -> Left $ "runTestlike: error in callee:\n"
          ++ show (map (fromLeft "") lefts)
  True -> Right $ reconcileCondElts $ S.fromList
           $ map (fromRight M.empty) results

runTestlike d p (QOr qs) s ce = let
  results = map (\q -> runTestlike d p q s ce) qs :: [Either String CondElts]
  lefts = filter isLeft results
  in case null lefts of
  False -> Left $ "runTestlike: error in callee:\n"
          ++ show (map (fromLeft "") lefts)
  True -> Right $ M.unionsWith S.union
           $ map (fromRight M.empty) results

runTestlike d p (ForSome v q) s ce =
  let errMsg = "runTestlike: error in callee:\n"
  in case extendPossible v p s of
  Left s -> Left $ errMsg ++ s
  Right (p',ss) -> let
    res :: Set (Either String CondElts)
    res = S.map (flip (runTestlike d p' q) ce) ss
    lefts = S.filter isLeft res
    in case null lefts of
    False -> Left $ errMsg ++ show (S.map (fromLeft "") lefts)
    True -> Right $ M.unionsWith S.union
            $ S.map (fromRight M.empty) res

runTestlike d p (ForAll v q) s ce = let
  errMsg = "runTestlike: error in callee:\n"
  in case extendPossible v p s of
  Left s -> Left $ errMsg ++ s
  Right (p',ss) -> let
    res :: Set (Either String CondElts)
    res = S.map (flip (runTestlike d p' q) ce) ss
    lefts = S.filter isLeft res
    in case null lefts of
    False -> Left $ errMsg ++ show (S.map (fromLeft "") lefts)
    True -> let
      cesWithoutV :: Set CondElts
      cesWithoutV = S.map f res where
        -- delete the dependency on v, so that reconciliation can work
        f = (M.map $ S.map $ M.delete v) . (fromRight M.empty)
      in Right $ reconcileCondElts cesWithoutV
         -- keep only results that obtain for every value of v


-- | = runFindlike

runFindlike :: Data
         -> Possible -- ^ how the `Program`'s earlier `Var`s have been bound
         -> Query
         -> Subst  -- ^ earlier (higher, calling) quantifiers draw these
                   -- from the input `Possible`
         -> Either String CondElts

runFindlike _ _ (findlike -> False) _ = Left "runFindlike: non-findlike Query"
runFindlike d _ (QFind f) s = Right $ runFind d s f
runFindlike d p (QAnd qs) s = runQAnd d p qs s

runFindlike d p (QOr qs) s = let
  -- TODO (#fast|#hard) Fold QOr with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
  ces = map (flip (runFindlike d p) s) qs :: [Either String CondElts]
  lefts = filter isLeft ces
  in case null lefts of
  False -> Left $ "runFindlike: error in callee:\n"
    ++ show (map (fromLeft "") lefts)
  True -> Right $ M.unionsWith S.union $ map (fromRight M.empty) ces

runFindlike d p (ForSome v q) s =
  case extendPossible v p s of
  Left s -> Left $ "runFindlike: error in callee:\n" ++ s
  Right (p',ss) -> let
    (res   :: Set (Either String CondElts)) = S.map (runFindlike d p' q) ss
    (lefts :: Set (Either String CondElts)) = S.filter isLeft res
    in case null lefts of
    False -> Left $ "runTestlike: error(s) in callee:\n"
      ++ show (S.map (fromLeft "") lefts)
    True -> Right $ M.unionsWith S.union
            $ S.map (fromRight M.empty) res

  -- TODO (#fast) runFindlike: Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.

runFindlike d p (ForAll v q) s =
  case extendPossible v p s of
  Left s -> Left $ "runFindlike: error in callee:\n" ++ s
  Right (p',ss) -> let
    (res   :: Set (Either String CondElts)) = S.map (runFindlike d p' q) ss
    (lefts :: Set (Either String CondElts)) = S.filter isLeft res
    in case null lefts of
    False -> Left $ "runTestlike: error(s) in callee:\n"
      ++ show (S.map (fromLeft "") lefts)
    True -> let
      (cesWithoutV :: Set CondElts) = S.map f res where
        -- delete the dependency on v, so that reconciliation can work
        f = (M.map $ S.map $ M.delete v) . (fromRight M.empty)
      in Right $ reconcileCondElts cesWithoutV
         -- keep only results that obtain for every value of v
