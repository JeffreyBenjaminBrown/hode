{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Hode.Qseq.Query (
    runProgram            -- | sp               -> [(Var,Query e sp)]
                          --               -> Either String (Possible e)
  , runAnd                -- | sp -> Possible e -> Subst e -> [Query e sp]
                          --               -> Either String (CondElts e)
  , runVarTestlike        -- | sp -> Possible e -> Subst e -> Query e sp
                          --               -> Either String Bool
  , substsThatPassVarTest -- | sp -> Possible e -> Query e sp -> [Subst e]
                          --               -> Either String [Subst e]
  , runTestlike           -- | sp -> Possible e -> CondElts e -> Subst e
                          -- -> Query e sp -> Either String (CondElts e)
  , runFindlike           -- | sp -> Possible e -> Subst e -> Query e sp
                          --               -> Either String (CondElts e)
  ) where

import           Data.List
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Qseq.Valid
import Hode.Qseq.RunLeaf
import Hode.Qseq.Subst
import Hode.Qseq.Types
import Hode.Util.Misc


-- | == `runProgram` runs a sequence of `Query`s.

runProgram :: forall e sp. (Ord e, Show e)
  => sp
  -> [(Var,Query e sp)] -- ^ ordered: `Query`s can depend on earlier ones
  -> Either String (Possible e)

runProgram sp vqs =
  prefixLeft "runProgram:" $
  case validProgram vqs of
    Left s   -> Left s
    Right () -> foldl go (Right M.empty) vqs
      where
      go :: Either String (Possible e) -> (Var, Query e sp)
         -> Either String (Possible e)
      go (Left s) _ = Left s
      go (Right p) (v,q) = do
        (ec :: CondElts e) <- prefixLeft "runProgram:"
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
runAnd sp p s qs =
  let calldata = "-> runAdn, with subst " ++ show s
      (searches,tests') = partition findlike qs
      (tests,varTests) = partition testlike tests'

  in prefixLeft calldata $ do
    (varTestResults :: [Bool]) <-
      prefixLeft "at varTestResults:" $ ifLefts
      $ map (runVarTestlike sp p s) varTests
    if not $ and varTestResults
      then Right M.empty
      else do
      (found :: [CondElts e]) <-
        prefixLeft "at found:" $ ifLefts
        $ map (runFindlike sp p s) searches

      let (reconciled ::  CondElts e) = reconcileCondElts $ S.fromList found
          tested = foldr f (Right reconciled) tests where
            f :: Query e sp -> Either String (CondElts e)
                            -> Either String (CondElts e)
            f _ (Left msg) =
              Left $ calldata ++ ", at tested --called-> " ++ msg
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
  Left $ ( "-> runVarTestlike, called with subst " ++ show s
           ++ ": not a varTestlike Query." )
runVarTestlike _ _ s (QFind _) =
  Left $ ( "-> runVarTestlike, called with subst " ++ show s
           ++ ": on a QFind (which is not a varTestlike Query)." )
runVarTestlike _ _ s (QTest _) =
  Left $ ( "-> runVarTestlike, called with subst " ++ show s
           ++ ": on a QTest (which is not a varTestlike Query)." )

runVarTestlike sp p s (QVTest vtest) =
  prefixLeft "runVarTestlike:" $ runVarTest p sp s vtest

runVarTestlike sp p s (QJunct (QAnd vtests)) =
  prefixLeft "runVarTestlike:" $
  and <$> ( ifLefts
            $ map (runVarTestlike sp p s) vtests )

runVarTestlike sp p s (QJunct (QOr vtests)) =
  prefixLeft "runVarTestlike:" $
  or <$> ( ifLefts
           $ map (runVarTestlike sp p s) vtests)

runVarTestlike sp p s0 (QQuant w) =
  prefixLeft "runVarTestlike:" $ do
    (ss :: [Subst e]) <- S.toList  <$>
                         ( prefixLeft "at ss:"
                           $ drawVar p s0 (source w) (name w) )
    (conditioned :: [Subst e]) <-
      prefixLeft "at conditioned:"
      $ substsThatPassVarTest sp p (condition w) ss
    (bs :: [Bool]) <-
      prefixLeft "at bs:" $ ifLefts
      $ map (\s -> runVarTestlike sp p s $ goal w) conditioned
    Right $ case w of ForSome _ _ _   -> or  bs
                      ForAll  _ _ _ _ -> and bs

substsThatPassVarTest :: (Ord e, Show e) =>
  sp -> Possible e -> Query e sp -> [Subst e]
  -> Either String [Subst e]
substsThatPassVarTest sp p vtest ss =
  prefixLeft "substsThatPassVarTest:" $ do
    whetherEachPasses <-
      ifLefts $ map (\s -> runVarTestlike sp p s vtest) ss
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
  Left $ "-> runTestlike: not a testlike Query"
runTestlike _ _ _ _ (QFind _) =
  Left $ "-> runTestlike: not a testlike Query"
runTestlike sp _ ce s (QTest t) = runTest sp s t ce
runTestlike _ _ _ _ (QVTest _) =
  Left $ "-> runTestlike: VarTest should have been handled by And."

runTestlike sp p ce s (QJunct (QAnd qs)) =
  prefixLeft "runTestlike:" $ do
    (results :: [CondElts e]) <-
      ifLefts $ map (runTestlike sp p ce s) qs
    Right $ reconcileCondElts $ S.fromList results

runTestlike sp p ce s (QJunct (QOr qs)) =
  prefixLeft "runTestlike:" $ do
    (results :: [CondElts e]) <- ifLefts
      $ map (runTestlike sp p ce s) qs
    Right $ M.unionsWith S.union results

runTestlike sp p ce s (QQuant (ForSome v src q)) =
  prefixLeft "runTestlike:" $ do
    (ss :: Set (Subst e))     <- drawVar p s src v
    (res :: Set (CondElts e)) <- ifLefts_set
      $ S.map (flip (runTestlike sp p ce) q) ss
    Right $ M.unionsWith S.union res

runTestlike sp p ce s (QQuant (ForAll v src conds q)) =
  prefixLeft "runTestlike:" $ do
    (ss :: Set (Subst e)) <- prefixLeft "runTestlike, at ss:"
                             $ drawVar p s src v
    (conditioned :: Set (Subst e)) <-
      S.fromList <$>
      ( prefixLeft "at conditioned:"
        $ substsThatPassVarTest sp p conds (S.toList ss) )
    (tested :: Set (CondElts e)) <- prefixLeft "at testsed:"
      $ ifLefts_set
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

runFindlike sp _ s (QFind f) = prefixLeft "runFindlike:" $ runFind sp s f

runFindlike sp p s (QJunct (QAnd qs)) =
  prefixLeft "runFindlike, called on QAnd:"
  $ runAnd sp p s qs

runFindlike sp p s (QJunct (QOr qs)) =
  prefixLeft "runFindlike, called on QOr:" $ do
  -- TODO (#fast|#hard) Fold Or with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
    (ces :: [CondElts e]) <- ifLefts $ map (runFindlike sp p s) qs
    Right $ M.unionsWith S.union ces

runFindlike sp p s (QQuant (ForSome v src q)) =
  prefixLeft "runFindlike:" $ do
    let calldata = "called on ForSome, with arguments s= "
                   ++ show s ++ ", v= " ++ show v ++ ", src= " ++ show src
    ss <- prefixLeft  (calldata ++ ", at ss")
          $ drawVar p s src v
    (res :: Set (CondElts e)) <-
      prefixLeft "at res:" $ ifLefts_set
      $ S.map (flip (runFindlike sp p) q) ss
    Right $ M.unionsWith S.union res

  -- TODO (#fast) runFindlike: Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.

runFindlike sp p s (QQuant (ForAll v src conds q)) =
  let calldata = "-> runFindlike, called on ForAll, with arguments s= "
                 ++ show s ++ ", v= " ++ show v ++ ", src= " ++ show src
  in prefixLeft calldata $ do
    (ss :: Set (Subst e)) <- prefixLeft "at ss:"
                             $ drawVar p s src v
    (conditioned :: Set (Subst e)) <-
      S.fromList <$>
      ( prefixLeft "at conditioned:"
        $ substsThatPassVarTest sp p conds (S.toList ss) )
    (found :: Set (CondElts e))  <-
      prefixLeft "at found:"
      $ ifLefts_set $ S.map (flip (runFindlike sp p) q) conditioned
    let (foundWithoutV :: Set (CondElts e)) =
          S.map f found where
          -- delete the dependency on v, so that reconciliation can work
          f = M.map $ S.map $ M.delete v
    Right $ reconcileCondElts foundWithoutV
      -- keep only results that obtain for every value of v
