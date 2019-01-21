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
import Subst
import Types
import Util


-- | == Atomic queries

-- | = Building atomic queries

unEitherEltVar :: Subst e -> Either e Var -> (Maybe e, Maybe Var)
unEitherEltVar _ (Left e) = (Just e, Nothing)
unEitherEltVar s (Right v) = (M.lookup v s, Just v)


-- | = `Test`s

mkTest :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Test e sp
mkTest compare eev = Test go deps where
  go :: sp -> Subst e -> e -> Bool
  go _ s = let (me, mv) = unEitherEltVar s eev :: (Maybe e, Maybe Var)
               err = error $ keyErr "isNot_1" (fromJust mv) s
           in maybe err compare me
  deps :: Set Var
  deps = S.fromList $ catMaybes $ map (either (const Nothing) Just) [eev]


-- | = `VarTest`s

mkVarTest :: forall e sp. (Eq e, Show e)
        => (e -> e -> Bool) -> Either e Var -> Either e Var -> VarTest e sp
mkVarTest compare eev eev' = VarTest go deps where
  go :: sp -> Subst e -> Bool
  go _ s = let (me , mv ) = unEitherEltVar s eev  :: (Maybe e, Maybe Var)
               (me', mv') = unEitherEltVar s eev' :: (Maybe e, Maybe Var)
               err  = error $ keyErr "isNot_2" (fromJust mv ) s
               err' = error $ keyErr "isNot_2" (fromJust mv') s
    in case me of Nothing -> err
                  Just e -> case me' of Nothing -> err'
                                        Just e' -> compare e e'
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']


-- | = `Find`s

-- | Run a search that does not depend on the `Subst`.
mkFind :: forall e sp. (Show e) =>
  String -> (sp ->      Set e) ->                 Find e sp
-- | Run a search starting from some element of the space.
mkFindFrom :: forall e sp. (Show e) =>
  String -> (sp -> e -> Set e) -> Either e Var -> Find e sp

mkFind findName finder = Find go S.empty where
  go :: sp -> Subst e -> Set e
  go g _ = finder g
mkFindFrom findName finder eev = Find go deps where
  go :: sp -> Subst e -> Set e
  go g s = maybe err (finder g) me where
      (me, mv) = unEitherEltVar s eev :: (Maybe e, Maybe Var)
      err = error $ keyErr findName (fromJust mv) s
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev]


-- | = Running atomic queries

runVarTest :: sp -> Subst e -> VarTest e sp -> Bool
runVarTest d s t = (varTestFunction t) d s

runFind :: forall e sp. sp -> Subst e -> Find e sp -> CondElts e
runFind d s (Find find deps) =
  M.fromSet (const $ S.singleton used) found
  where
    found = find d s             :: Set e
    used = M.restrictKeys s deps :: Subst e

runTestOnElt :: forall e sp.
                sp -> Subst e -> Test e sp -> e -> (Bool, Subst e)
runTestOnElt d s (Test test deps) e = (passes, used)
  where
    passes = test d s e          :: Bool
    used = M.restrictKeys s deps :: Subst e

runTest :: forall e sp. Ord e
        => sp -> Subst e -> Test e sp -> CondElts e -> CondElts e
runTest d s q ce = let
  passed :: Map e (Bool, Subst e)
  passed = M.filter fst tested where
    tested = M.mapWithKey (\k v -> runTestOnElt d s q k) ce
  f ::  e -> (Bool, Subst e) -> Set (Subst e)
  f k (_,s) = let ss = (M.!) ce k :: Set (Subst e)
              in S.map (M.union s) ss
  in M.mapWithKey f passed


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
        => sp -> Possible e -> [Query e sp] -> Subst e
        -> Either String (CondElts e)
runAnd d p qs s = do
  let errMsg = "runAnd: error in callee:\n"
      (searches,tests') = partition findlike qs
      (varTests,tests) = partition (\case QVTest _->True; _->False) tests'
      varTestResults = map (runVarTest d s . unwrap) varTests where
        unwrap = \case QVTest t->t; _->error "runAnd: unwrap: impossible."
  if not $ and varTestResults then Right M.empty
  else do
   (found :: [CondElts e]) <- hopeNoLefts errMsg
                              $ map (flip (runFindlike d p) s) searches
   let (reconciled ::  CondElts e) = reconcileCondElts $ S.fromList found
       tested = foldr f (Right reconciled) tests where
         f :: Query e sp -> Either String (CondElts e)
                         -> Either String (CondElts e)
         f _ (Left s) = Left $ errMsg ++ s -- collect no further errors
         f t (Right ce) = runTestlike d p t s ce
   tested


-- | = runTestlike

-- TODO (#fast) runTestlike: foldr with short-circuiting.
runTestlike :: forall e sp. (Ord e, Show e)
            => sp -> Possible e -> Query e sp -> Subst e -> CondElts e
            -> Either String (CondElts e)
runTestlike _ _ (testlike -> False) _ _ =
  Left $ "runTestlike: not a testlike Query"
runTestlike d _ (QTest t) s ce = Right $ runTest d s t ce
runTestlike _ _ (QVTest _) _ _ =
  Left $ "runTestlike: VarTest should have been handled by And."

runTestlike d p (QJunct (And qs)) s ce = do
  (results :: [CondElts e]) <-
    hopeNoLefts "runTestlike: error in callee:\n"
    $ map (\q -> runTestlike d p q s ce) qs
  Right $ reconcileCondElts $ S.fromList results

runTestlike d p (QJunct (Or qs)) s ce = do
  (results :: [CondElts e]) <-
    hopeNoLefts "runTestlike: error in callee:\n"
    $ map (\q -> runTestlike d p q s ce) qs
  Right $ M.unionsWith S.union results

runTestlike d p (QQuant (ForSome v src q)) s ce = do
  let errMsg = "runTestlike: error in callee:\n"
  ((p',ss) :: (Possible e, Set (Subst e))) <-
    either (\s -> Left $ errMsg ++ s) Right
    $ extendPossible v src p s
  (res :: Set (CondElts e)) <-
    hopeNoLefts_set errMsg
    $ S.map (flip (runTestlike d p' q) ce) ss
  Right $ M.unionsWith S.union res

runTestlike d p (QQuant (ForAll v src q)) s ce = do
  let errMsg = "runTestlike: error in callee:\n"
  ((p',ss) :: (Possible e, Set (Subst e))) <-
    either (\s -> Left $ errMsg ++ s) Right
    $ extendPossible v src p s
  (res :: Set (CondElts e)) <-
    hopeNoLefts_set errMsg
    $ S.map (flip (runTestlike d p' q) ce) ss

  let cesWithoutV :: Set (CondElts e)
      cesWithoutV = S.map f res where
        -- delete the dependency on v, so that reconciliation can work
        f = M.map $ S.map $ M.delete v
  Right $ reconcileCondElts cesWithoutV
         -- keep only results that obtain for every value of v


-- | = runFindlike

runFindlike :: forall e sp. (Ord e, Show e)
         => sp
         -> Possible e  -- ^ bindings of the `Program`'s earlier `Var`s
         -> Query e sp
         -> Subst e  -- ^ earlier (higher, calling) quantifiers draw these
                     -- from the input `Possible`
         -> Either String (CondElts e)

runFindlike _ _ (findlike -> False) _ = Left "runFindlike: non-findlike Query"
runFindlike d _ (QFind f) s = Right $ runFind d s f
runFindlike d p (QJunct (And qs)) s = runAnd d p qs s

runFindlike d p (QJunct (Or qs)) s = do
  -- TODO (#fast|#hard) Fold Or with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
  (ces :: [CondElts e]) <-
    hopeNoLefts "runFindlike: error in callee:\n"
    $ map (flip (runFindlike d p) s) qs
  Right $ M.unionsWith S.union ces

runFindlike d p (QQuant (ForSome v src q)) s = do
  ((p',ss) :: (Possible e, Set (Subst e))) <-
    either (\s -> Left $ "runFindlike: error in callee:\n" ++ s) Right
    $ extendPossible v src p s
  (res :: Set (CondElts e)) <-
    hopeNoLefts_set "runFindlike: error(s) in callee:\n"
    $ S.map (runFindlike d p' q) ss
  Right $ M.unionsWith S.union res

  -- TODO (#fast) runFindlike: Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.

runFindlike d p (QQuant (ForAll v src q)) s = do
  ((p',ss) :: (Possible e, Set (Subst e))) <-
    either (\s -> Left $ "runFindlike: error in callee:\n" ++ s) Right
    $ extendPossible v src p s
  (res :: Set (CondElts e)) <-
    hopeNoLefts_set "runFindlike: error(s) in callee:\n"
    $ S.map (runFindlike d p' q) ss
  let (cesWithoutV :: Set (CondElts e)) =
        S.map f res where
        -- delete the dependency on v, so that reconciliation can work
        f = M.map $ S.map $ M.delete v
  Right $ reconcileCondElts cesWithoutV
    -- keep only results that obtain for every value of v
