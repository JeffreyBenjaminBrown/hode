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

import Query.Inspect
import Query.RunLeaf
import Subst
import Types
import Util


-- | == Complex queries

---- | = runAnd
---- There are three types of queries: testlike, findlike, and VarTests.
---- varTests and testlike queries can only be run from a conjunction (And).
---- A conjunction runs all varTests first, because they are simplest: they
---- don't require reference to the Possible, and often not the data either.
---- If all Vars in the Subst pass all VarTests, then the findlike queries are
---- run. Once those results are collected, they are filtered by the testlike
---- queries.
--
--runAnd :: forall e sp. (Ord e, Show e)
--        => sp -> Possible e -> [Query e sp] -> Subst e
--        -> Either String (CondElts e)
--runAnd d p qs s = do
--  let errMsg = "runAnd: error in callee:\n"
--      (searches,tests') = partition findlike qs
--      (varTests,tests) = partition (\case QVTest _->True; _->False) tests'
--      varTestResults = map (runVarTest d s . unwrap) varTests where
--        unwrap = \case QVTest t->t; _->error "runAnd: unwrap: impossible."
--  if not $ and varTestResults then Right M.empty
--  else do
--   (found :: [CondElts e]) <- ifLefts errMsg
--                              $ map (flip (runFindlike d p) s) searches
--   let (reconciled ::  CondElts e) = reconcileCondElts $ S.fromList found
--       tested = foldr f (Right reconciled) tests where
--         f :: Query e sp -> Either String (CondElts e)
--                         -> Either String (CondElts e)
--         f _ (Left s) = Left $ errMsg ++ s -- collect no further errors
--         f t (Right ce) = runTestlike d p t s ce
--   tested


-- | = runTestlike

-- TODO (#fast) runTestlike: foldr with short-circuiting.
runTestlike :: forall e sp. (Ord e, Show e)
            => sp
            -> Possible e
            -> Subst e
            -> CondElts e
            -> Query e sp
            -> Either String (CondElts e)

runTestlike _ _ _ _ (testlike -> False) =
  Left $ "runTestlike: not a testlike Query"
runTestlike d _ s ce (QTest t) = Right $ runTest d s t ce
runTestlike _ _ _ _ (QVTest _) =
  Left $ "runTestlike: VarTest should have been handled by And."

runTestlike d p s ce (QJunct (And qs)) = do
  (results :: [CondElts e]) <-
    ifLefts "runTestlike: error in callee:\n"
    $ map (runTestlike d p s ce) qs
  Right $ reconcileCondElts $ S.fromList results

runTestlike d p s ce (QJunct (Or qs)) = do
  (results :: [CondElts e]) <-
    ifLefts "runTestlike: error in callee:\n"
    $ map (runTestlike d p s ce) qs
  Right $ M.unionsWith S.union results

runTestlike d p s ce (QQuant (ForSome v src q)) = do
  let errMsg = "runTestlike: error in callee:\n"
  ss <- either (\msg -> Left $ errMsg ++ msg) Right
    $ drawVar p s src v
  (res :: Set (CondElts e)) <-
    ifLefts_set errMsg
    $ S.map (\s -> runTestlike d p s ce q) ss
  Right $ M.unionsWith S.union res

runTestlike d p s ce (QQuant (ForAll v src q vtests)) = do
  let errMsg = "runTestlike: error in callee:\n"
  (ss :: Set (Subst e)) <-
    either (\msg -> Left $ errMsg ++ msg) Right
    $ drawVar p s src v
  let (varTested :: Set (Subst e)) =
        S.filter passesAllVarTests ss where
        passesAllVarTests :: Subst e -> Bool
        passesAllVarTests s = and $ map (runVarTest d s) vtests
  (tested :: Set (CondElts e)) <-
    ifLefts_set errMsg
    $ S.map (\s -> runTestlike d p s ce q) varTested
  let (cesWithoutV :: Set (CondElts e)) = S.map f tested where
        -- delete the dependency on v, so that reconciliation can work
        f = M.map $ S.map $ M.delete v
  Right $ reconcileCondElts cesWithoutV
        -- keep only results that obtain for every value of v


---- | = runFindlike
--
--runFindlike :: forall e sp. (Ord e, Show e)
--         => sp
--         -> Possible e  -- ^ bindings of the `Program`'s earlier `Var`s
--         -> Query e sp
--         -> Subst e  -- ^ earlier (higher, calling) quantifiers draw these
--                     -- from the input `Possible`
--         -> Either String (CondElts e)
--
--runFindlike _ _ (findlike -> False) _ = Left "runFindlike: non-findlike Query"
--runFindlike d _ (QFind f) s = Right $ runFind d s f
--runFindlike d p (QJunct (And qs)) s = runAnd d p qs s
--
--runFindlike d p (QJunct (Or qs)) s = do
--  -- TODO (#fast|#hard) Fold Or with short-circuiting.
--  -- Once an `Elt` is found, it need not be searched for again, unless
--  -- a new `Subst` would be associated with it.
--  (ces :: [CondElts e]) <-
--    ifLefts "runFindlike: error in callee:\n"
--    $ map (flip (runFindlike d p) s) qs
--  Right $ M.unionsWith S.union ces
--
--runFindlike d p (QQuant (ForSome v src q)) s = do
--  ss <- either (\msg -> Left $ "runFindlike:\n" ++ msg) Right
--    $ drawVar p s src v
--  (res :: Set (CondElts e)) <-
--    ifLefts_set "runFindlike: error(s) in callee:\n"
--    $ S.map (runFindlike d p q) ss
--  Right $ M.unionsWith S.union res
--
--  -- TODO (#fast) runFindlike: Fold ForAll with short-circuiting.
--  -- Once an Elt fails to obtain for one value of v,
--  -- don't search for it using any remaining value of v.
--
--runFindlike d p (QQuant (ForAll v src q vtests)) s = do
--  ss <- either (\msg -> Left $ "runFindlike:\n" ++ msg) Right
--    $ drawVar p s src v
--  (res :: Set (CondElts e)) <-
--    ifLefts_set "runFindlike: error(s) in callee:\n"
--    $ S.map (runFindlike d p q) ss
--  let (cesWithoutV :: Set (CondElts e)) =
--        S.map f res where
--        -- delete the dependency on v, so that reconciliation can work
--        f = M.map $ S.map $ M.delete v
--  Right $ reconcileCondElts cesWithoutV
--    -- keep only results that obtain for every value of v
