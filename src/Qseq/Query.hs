{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Qseq.Query where

import           Data.List
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QValid
import Qseq.RunLeaf
import Qseq.Subst
import Qseq.QTypes
import Util.Misc


-- | == `runProgram` runs a sequence of `Query`s.

runProgram :: forall e sp. (Ord e, Show e)
  => sp
  -> [(Var,Query e sp)] -- ^ ordered: `Query`s can depend on earlier ones
  -> Either String (Possible e)

runProgram sp vqs = case validProgram vqs of
  Left s   -> Left s
  Right () -> foldl go (Right M.empty) vqs
    where
    go :: Either String (Possible e) -> (Var, Query e sp)
       -> Either String (Possible e)
    go (Left s) _ = Left s
    go (Right p) (v,q) = do
      (ec :: CondElts e) <- prefixLeft "runProgram"
                            $ runFindlike sp p (M.empty :: Subst e) q
      Right $ M.insert v ec p


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
runAnd sp p s qs = do
  let calldata = "runAdn, with subst " ++ show s
      (searches,tests') = partition findlike qs
      (tests,varTests) = partition testlike tests'
  (varTestResults :: [Bool]) <- ifLefts ( calldata ++ ", at varTestResults" )
                                $ map (runVarTestlike sp p s) varTests
  if not $ and varTestResults
    then Right M.empty
    else do
    (found :: [CondElts e]) <- ifLefts ( calldata ++ ", at found" )
                               $ map (runFindlike sp p s) searches

    let (reconciled ::  CondElts e) = reconcileCondElts $ S.fromList found
        tested = foldr f (Right reconciled) tests where
          f :: Query e sp -> Either String (CondElts e)
                          -> Either String (CondElts e)
          f _ (Left msg) = Left $ calldata ++ ", at tested --called-> " ++ msg
                         -- collect no further Lefts
          f t (Right ce) = runTestlike sp p ce s t
    tested


-- | = runVarTestlike

runVarTestlike :: forall e sp. (Ord e, Show e)
  => sp
  -> Possible e
  -> Subst e
  -> Query e sp
  -> Either String Bool

runVarTestlike _ _ s (varTestlike -> False) =
  Left $ ( "runVarTestlike, called with subst " ++ show s
           ++ ": not a varTestlike Query." )
runVarTestlike _ _ s (QFind _) =
  Left $ ( "runVarTestlike, called with subst " ++ show s
           ++ ": on a QFind (which is not a varTestlike Query)." )
runVarTestlike _ _ s (QTest _) =
  Left $ ( "runVarTestlike, called with subst " ++ show s
           ++ ": on a QTest (which is not a varTestlike Query)." )

runVarTestlike sp p s (QVTest vtest) =
  prefixLeft "runVarTestlike" $ runVarTest p sp s vtest

runVarTestlike sp p s (QJunct (QAnd vtests)) =
  and <$>
  ( ifLefts "runVarTestlike"
    $ map (runVarTestlike sp p s) vtests )

runVarTestlike sp p s (QJunct (QOr vtests)) =
  or <$>
  ( ifLefts "runVarTestlike"
    $ map (runVarTestlike sp p s) vtests )

runVarTestlike sp p s0 (QQuant w) = do
  (ss :: [Subst e]) <- S.toList  <$>
                       ( prefixLeft "runVarTestlike, at ss"
                         $ drawVar p s0 (source w) (name w) )
  (conditioned :: [Subst e]) <-
    prefixLeft "runVarTestlike, at conditioned"
    $ substsThatPassVarTest sp p (condition w) ss
  (bs :: [Bool]) <-
    ifLefts "runVarTestlike, at bs"
    $ map (\s -> runVarTestlike sp p s $ goal w) conditioned
  Right $ case w of ForSome _ _ _   -> or  bs
                    ForAll  _ _ _ _ -> and bs

substsThatPassVarTest :: (Ord e, Show e) =>
  sp -> Possible e -> Query e sp -> [Subst e]
  -> Either String [Subst e]
substsThatPassVarTest sp p vtest ss = do
  whetherEachPasses <-
    ifLefts "substsThatPassVarTest, at whetherEachPasses"
    $ map (\s -> runVarTestlike sp p s vtest) ss
  Right $ map fst $ filter snd $ zip ss whetherEachPasses


-- | = runTestlike

-- TODO (#fast) runTestlike: foldr with short-circuiting.
runTestlike :: forall e sp. (Ord e, Show e)
            => sp
            -> Possible e
            -> CondElts e -- ^ results of at least one `Find`
            -> Subst e
            -> Query e sp
            -> Either String (CondElts e)

runTestlike _ _ _ _ (testlike -> False) =
  Left $ "runTestlike: not a testlike Query"
runTestlike _ _ _ _ (QFind _) =
  Left $ "runTestlike: not a testlike Query"
runTestlike sp _ ce s (QTest t) = runTest sp s t ce
runTestlike _ _ _ _ (QVTest _) =
  Left $ "runTestlike: VarTest should have been handled by And."

runTestlike sp p ce s (QJunct (QAnd qs)) = do
  (results :: [CondElts e]) <-
    ifLefts "runTestlike"
    $ map (runTestlike sp p ce s) qs
  Right $ reconcileCondElts $ S.fromList results

runTestlike sp p ce s (QJunct (QOr qs)) = do
  (results :: [CondElts e]) <- ifLefts "runTestlike"
                               $ map (runTestlike sp p ce s) qs
  Right $ M.unionsWith S.union results

runTestlike sp p ce s (QQuant (ForSome v src q)) = do
  (ss :: Set (Subst e))     <- prefixLeft  "runTestlike"
                               $ drawVar p s src v
  (res :: Set (CondElts e)) <- ifLefts_set "runTestlike"
                               $ S.map (flip (runTestlike sp p ce) q) ss
  Right $ M.unionsWith S.union res

runTestlike sp p ce s (QQuant (ForAll v src conds q)) = do
  (ss :: Set (Subst e))        <- prefixLeft "runTestlike, at ss"
                                  $ drawVar p s src v
  (conditioned :: Set (Subst e)) <-
    S.fromList <$>
    ( prefixLeft "runTestlike, at conditioned"
      $ substsThatPassVarTest sp p conds (S.toList ss) )
  (tested :: Set (CondElts e)) <- ifLefts_set "runTestlike, at testsed"
    $ S.map (flip (runTestlike sp p ce) q) conditioned
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
runFindlike _ _ _ (QTest _)           = Left "runFindlike: non-findlike Query"
runFindlike _ _ _ (QVTest _)          = Left "runFindlike: non-findlike Query"

runFindlike sp _ s (QFind f) = prefixLeft "runFindlike" $ runFind sp s f

runFindlike sp p s (QJunct (QAnd qs)) =
  prefixLeft "runFindlike, called on QAnd"
  $ runAnd sp p s qs

runFindlike sp p s (QJunct (QOr qs)) = do
  -- TODO (#fast|#hard) Fold Or with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
  (ces :: [CondElts e]) <- ifLefts "runFindlike, call on QOr"
                           $ map (runFindlike sp p s) qs
  Right $ M.unionsWith S.union ces

runFindlike sp p s (QQuant (ForSome v src q)) = do
  let calldata = "runFindlike, called on ForSome, with arguments s: "
                 ++ show s ++ ", v: " ++ show v ++ ", src: " ++ show src
  ss <- prefixLeft  (calldata ++ ", at ss")
        $ drawVar p s src v
  (res :: Set (CondElts e)) <-
    ifLefts_set ( calldata ++ ", at res" )
    $ S.map (flip (runFindlike sp p) q) ss
  Right $ M.unionsWith S.union res

  -- TODO (#fast) runFindlike: Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.

runFindlike sp p s (QQuant (ForAll v src conds q)) = do
  let calldata = "runFindlike, called on ForAll, with arguments s: "
                 ++ show s ++ ", v: " ++ show v ++ ", src: " ++ show src
  (ss :: Set (Subst e)) <- prefixLeft (calldata ++ ", at ss")
                           $ drawVar p s src v
  (conditioned :: Set (Subst e)) <-
    S.fromList <$>
    ( prefixLeft (calldata ++ ", at conditioned")
      $ substsThatPassVarTest sp p conds (S.toList ss) )
  (found :: Set (CondElts e))  <-
    ifLefts_set (calldata ++ ", at found")
    $ S.map (flip (runFindlike sp p) q) conditioned
  let (foundWithoutV :: Set (CondElts e)) =
        S.map f found where
        -- delete the dependency on v, so that reconciliation can work
        f = M.map $ S.map $ M.delete v
  Right $ reconcileCondElts foundWithoutV
    -- keep only results that obtain for every value of v

