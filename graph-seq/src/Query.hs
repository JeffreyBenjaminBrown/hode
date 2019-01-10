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


-- | = Testing that a Query is valid.

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, introduces `v`.
-- And every `Query` introduces whatever its subqueries introduces.
introduces :: Query -> Set Var
introduces (QOr  qs)      = S.unions    $ map introduces qs
introduces (QAnd qs)      = S.unions    $ map introduces qs
introduces (ForSome vf q) = S.insert vf $     introduces q
introduces (ForAll  vf q) = S.insert vf $     introduces q
introduces _ = S.empty

-- | The only way a `Var` could be bound is if it was introduced by
-- a `ForSome`.
couldBind :: Query -> Set Var
couldBind (QOr  qs)      = S.unions    $ map couldBind qs
couldBind (QAnd qs)      = S.unions    $ map couldBind qs
couldBind (ForSome vf q) = S.insert vf $     couldBind q
couldBind (ForAll  _  q) =                   couldBind q
couldBind _              = S.empty

-- | Every `QAnd` must include something `findable`, and
-- every `QOr` must be nonempty and consist entirely of `findable` queries.
findable, testable :: Query -> Bool
findable (QFind _)          = True
findable (QTest _)          = False
findable (QAnd qs)          = or  $ map findable qs
findable (QOr     [])       = False
findable (QOr     qs@(_:_)) = and $ map findable qs
findable (ForSome _ q)      = findable q
findable (ForAll  _ q)      = findable q

testable = not . findable

-- | A `Query` is only valid if no quantifier masks an earlier one,
-- and if no `
disjointExistentials :: Query -> Bool
disjointExistentials (ForSome vf q)
  = not $ S.member vf $ couldBind q
disjointExistentials (ForAll vf q)
  = not $ S.member vf $ couldBind q
disjointExistentials (QAnd qs) = snd $ foldr f (S.empty, True) qs
  where f :: Query -> (Set Var, Bool) -> (Set Var, Bool)
        f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
        f q (vs, True) = if S.disjoint vs $ couldBind q
                         then (S.union vs $ couldBind q, True)
                         else (S.empty, False)
disjointExistentials _ = True


-- | Running queries.

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

runQAnd :: Data -> Possible -> [Query] -> Subst -> CondElts
runQAnd d p qs s = tested where
  (searches,tests) = partition findable qs
  found = map (flip (runQuery d p) s) searches :: [CondElts]
  reconciled, tested :: CondElts
  reconciled = reconcileCondElts $ S.fromList found
  tested = foldr (\t ce -> runTestable d p t s ce) reconciled tests

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
