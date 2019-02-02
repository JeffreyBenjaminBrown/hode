{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Search.Query where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Search.Query.Valid
import Search.Query.RunLeaf
import Search.Subst
import Search.Types
import Util


-- | == Complex queries

-- | = runAnd
-- There are three types of queries: testlike, findlike, and VarTests.
-- varTests and testlike queries can only be run from a conjunction (And).
-- A conjunction runs all varTests first, because they are simplest: they
-- don't require reference to the Possible, and often not the data either.
-- If all Vars in the Subst pass all VarTests, then the findlike queries are
-- run. Once those results are collected, they are filtered by the testlike
-- queries.

runAnd :: forall e sp. (Ord e, Show e)
        => sp -> Possible e -> Subst e -> [Query e sp]
        -> Either String (CondElts e)
runAnd d p s qs = do
  let errMsg = "runAnd: error in callee:\n"
      (searches,tests') = partition findlike qs
      (varTests,tests) = partition (\case QVTest _->True; _->False) tests'
      varTestResults = map (runVarTest p d s . unwrap) varTests where
        unwrap = \case QVTest t->t; _->error "runAnd: unwrap: impossible."
  if not $ and varTestResults then Right M.empty
  else do
   (found :: [CondElts e]) <- ifLefts errMsg
                              $ map (runFindlike d p s) searches
   let (reconciled ::  CondElts e) = reconcileCondElts $ S.fromList found
       tested = foldr f (Right reconciled) tests where
         f :: Query e sp -> Either String (CondElts e)
                         -> Either String (CondElts e)
         f _ (Left s) = Left $ errMsg ++ s -- collect no further errors
         f t (Right ce) = runTestlike d p ce s t
   tested


-- | = runVarTestlike

runVarTestlike :: forall e sp. (Ord e, Show e)
  => sp
  -> Possible e
  -> Subst e
  -> Query e sp
  -> Either String Bool

runVarTestlike _ _ _ (varTestlike -> False) =
  Left "runVarTestlike: not a varTestlike Query"

runVarTestlike sp p s (QVTest vtest) =
  Right $ runVarTest p sp s vtest

runVarTestlike sp p s (QJunct (And vtests)) =
  and <$>
  ( ifLefts "runVarTestlike: error(s) in callee:\n"
    $ map (runVarTestlike sp p s) vtests )

runVarTestlike sp p s (QJunct (Or vtests)) =
  or <$>
  ( ifLefts "runVarTestlike: error(s) in callee:\n"
    $ map (runVarTestlike sp p s) vtests )

runVarTestlike _ _ _ (QQuant _) =
  error "todo ? write runVarTestlike for the QQuant case."

substsThatPassAllVarTests :: (Ord e, Show e) =>
  sp -> Possible e -> [Query e sp] -> [Subst e]
  -> Either String [Subst e]
substsThatPassAllVarTests sp p vtests ss = do

  let passesAllVarTests :: (Ord e, Show e) =>
        sp -> Possible e -> [Query e sp] -> Subst e -> Either String Bool
      passesAllVarTests sp p vtests s = do
        (bs :: [Bool]) <- ifLefts "passesAllVarTests: error(s) in callee:\n"
          $ map (runVarTestlike sp p s) vtests
        Right $ and bs

  whetherEachPasses <-
    ifLefts "substsThatPassAllVarTests: error(s) in callee:\n"
    $ map (passesAllVarTests sp p vtests) ss
  Right $ map fst $ filter snd $ zip ss whetherEachPasses


-- | = runTestlike

-- TODO (#fast) runTestlike: foldr with short-circuiting.
runTestlike :: forall e sp. (Ord e, Show e)
            => sp
            -> Possible e
            -> CondElts e
            -> Subst e
            -> Query e sp
            -> Either String (CondElts e)

runTestlike _ _ _ _ (testlike -> False) =
  Left $ "runTestlike: not a testlike Query"
runTestlike d _ ce s (QTest t) = Right $ runTest d s t ce
runTestlike _ _ _ _ (QVTest _) =
  Left $ "runTestlike: VarTest should have been handled by And."

runTestlike d p ce s (QJunct (And qs)) = do
  (results :: [CondElts e]) <-
    ifLefts "runTestlike: error in callee:\n"
    $ map (runTestlike d p ce s) qs
  Right $ reconcileCondElts $ S.fromList results

runTestlike d p ce s (QJunct (Or qs)) = do
  (results :: [CondElts e]) <-
    ifLefts "runTestlike: error in callee:\n"
    $ map (runTestlike d p ce s) qs
  Right $ M.unionsWith S.union results

runTestlike d p ce s (QQuant (ForSome v src q)) = do
  let errMsg = "runTestlike: error in callee:\n"
  ss <- either (\msg -> Left $ errMsg ++ msg) Right
    $ drawVar p s src v
  (res :: Set (CondElts e)) <-
    ifLefts_set errMsg
    $ S.map (flip (runTestlike d p ce) q) ss
  Right $ M.unionsWith S.union res

runTestlike d p ce s (QQuant (ForAll v src q vtests)) = do
  let errMsg = "runTestlike: error in callee:\n"
  (ss :: Set (Subst e)) <-
    either (\msg -> Left $ errMsg ++ msg) Right
    $ drawVar p s src v
  (varTested :: Set (Subst e)) <-
    S.fromList <$> substsThatPassAllVarTests d p vtests (S.toList ss)
  (tested :: Set (CondElts e)) <-
    ifLefts_set errMsg
    $ S.map (flip (runTestlike d p ce) q) varTested
  let (cesWithoutV :: Set (CondElts e)) = S.map f tested where
        -- delete the dependency on v, so that reconciliation can work
        f = M.map $ S.map $ M.delete v
  Right $ reconcileCondElts cesWithoutV
        -- keep only results that obtain for every value of v


-- | = runFindlike

runFindlike :: forall e sp. (Ord e, Show e)
         => sp
         -> Possible e  -- ^ bindings of the `Program`'s earlier `Var`s
         -> Subst e  -- ^ earlier (higher, calling) quantifiers draw these
                     -- from the input `Possible`
         -> Query e sp
         -> Either String (CondElts e)

runFindlike _ _ _ (findlike -> False) = Left "runFindlike: non-findlike Query"
runFindlike d _ s (QFind f) = Right $ runFind d s f

runFindlike d p s (QJunct (And qs)) = runAnd d p s qs

runFindlike d p s (QJunct (Or qs)) = do
  -- TODO (#fast|#hard) Fold Or with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
  (ces :: [CondElts e]) <-
    ifLefts "runFindlike: error in callee:\n"
    $ map (runFindlike d p s) qs
  Right $ M.unionsWith S.union ces

runFindlike d p s (QQuant (ForSome v src q)) = do
  ss <- either (\msg -> Left $ "runFindlike:\n" ++ msg) Right
    $ drawVar p s src v
  (res :: Set (CondElts e)) <-
    ifLefts_set "runFindlike: error(s) in callee:\n"
    $ S.map (flip (runFindlike d p) q) ss
  Right $ M.unionsWith S.union res

  -- TODO (#fast) runFindlike: Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.

runFindlike d p s (QQuant (ForAll v src q vtests)) = do
  (ss :: Set (Subst e)) <-
    either (\msg -> Left $ "runFindlike:\n" ++ msg) Right
    $ drawVar p s src v
  (varTested :: Set (Subst e)) <-
    S.fromList <$> substsThatPassAllVarTests d p vtests (S.toList ss)
  (found :: Set (CondElts e)) <-
    ifLefts_set "runFindlike: error(s) in callee:\n"
    $ S.map (flip (runFindlike d p) q) varTested
  let (foundWithoutV :: Set (CondElts e)) =
        S.map f found where
        -- delete the dependency on v, so that reconciliation can work
        f = M.map $ S.map $ M.delete v
  Right $ reconcileCondElts foundWithoutV
    -- keep only results that obtain for every value of v
