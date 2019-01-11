{-# LANGUAGE ViewPatterns #-}

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

runFind :: Data -> Subst -> Find -> CondElts
runFind d s (Find find deps) =
  M.fromSet (const $ S.singleton used) found
  where
    found = find d s             :: Set Elt
    used = M.restrictKeys s deps :: Subst

runTestOnElt :: Data -> Subst -> Test -> Elt -> (Bool, Subst)
runTestOnElt d s (Test test deps) e =
  (passes, used)
  where
    passes = test d s e          :: Bool
    used = M.restrictKeys s deps :: Subst

runTest :: Data -> Subst -> Test -> CondElts -> CondElts
runTest d s q ce =
  M.map ( S.singleton . snd) passed
  where
    tested, passed :: Map Elt (Bool, Subst)
    tested = M.mapWithKey (\k v -> runTestOnElt d s q k) ce
    passed = M.filter fst tested


-- | == Complex queries

runQAnd :: Data -> Possible -> [Query] -> Subst -> CondElts
runQAnd d p qs s = tested where
  (searches,tests) = partition findable qs
  found = map (flip (runFindable d p) s) searches :: [CondElts]
  reconciled, tested :: CondElts
  reconciled = reconcileCondElts $ S.fromList found
  tested = foldr (\t ce -> runTestable d p t s ce) reconciled tests


-- | = runTestable

-- TODO (#speed) runTestable: foldr with short-circuiting.
runTestable :: Data -> Possible -> Query -> Subst -> CondElts
            -> Either String CondElts
runTestable _ _ (testable -> False) _ _ =
  Left $ "runTestable: not a testable Query"
runTestable d _ (QTest t) s ce = Right $ runTest d s t ce

runTestable d p (QAnd qs) s ce = let
  results = map (\q -> runTestable d p q s ce) qs :: [Either String CondElts]
  lefts = filter isLeft results
  in case null lefts of
       True -> Left $ "runTestable: error in callee:\n"
               ++ show (map (fromLeft "") lefts)
       False -> Right $ reconcileCondElts $ S.fromList
                $ map (fromRight M.empty) results

runTestable d p (QOr qs) s ce = let
  results = map (\q -> runTestable d p q s ce) qs :: [Either String CondElts]
  lefts = filter isLeft results
  in case null lefts of
       True -> Left $ "runTestable: error in callee:\n"
               ++ show (map (fromLeft "") lefts)
       False -> Right $ M.unionsWith S.union
                $ map (fromRight M.empty) results

runTestable d p (ForSome v q) s ce =
  case extendPossible v p s of
    Left s -> Left $ "runTestable: error in callee:\n" ++ s
    Right (p',ss) -> let
      res :: Set (Either String CondElts)
      res = S.map (flip (runTestable d p' q) ce) ss
      lefts = S.filter isLeft res
      in case null lefts of
           False -> Left $ "runTestable: error(s) in callee:\n"
             ++ show (S.map (fromLeft "") lefts)
           True -> Right $ M.unionsWith S.union
                   $ S.map (fromRight M.empty) res

runTestable d p (ForAll v q) s ce =
  case extendPossible v p s of
    Left s -> Left $ "runTestable: error in callee:\n" ++ s
    Right (p',ss) -> let
      res :: Set (Either String CondElts)
      res = S.map (flip (runTestable d p' q) ce) ss
      lefts = S.filter isLeft res
      in case null lefts of

           False -> Left $ "runTestable: error(s) in callee:\n"
             ++ show (S.map (fromLeft "") lefts)
           True -> let
             cesWithoutV :: Set CondElts
             cesWithoutV = S.map f res where
               -- delete the dependency on v, so that reconciliation can work
               f = (M.map $ S.map $ M.delete v) . (fromRight M.empty)
             in Right $ reconcileCondElts cesWithoutV
                -- keep only results that obtain for every value of v


-- | = runFindable

runFindable :: Data
         -> Possible -- ^ how the `Program`'s earlier `Var`s have been bound
         -> Query
         -> Subst  -- ^ earlier (higher, calling) quantifiers draw these
                   -- from the input `Possible`
         -> CondElts

runFindable _ _ (findable -> False) _ = error "runFindable: non-findable Query"
runFindable d _ (QFind f) s = runFind d s f
runFindable d p (QAnd qs) s = runQAnd d p qs s

runFindable d p (QOr qs) s =
  -- TODO (#speed|#hard) Fold QOr with short-circuiting.
  -- Once an `Elt` is found, it need not be searched for again, unless
  -- a new `Subst` would be associated with it.
  let ces = map (flip (runFindable d p) s) qs :: [CondElts]
  in M.unionsWith S.union ces

runFindable d p (ForSome v q) s = let
  (p',ss) = extendPossible v p s
  in M.unionsWith S.union $ S.map (runFindable d p' q) ss

runFindable d p (ForAll v q) s = let
  -- TODO (#speed) Fold ForAll with short-circuiting.
  -- Once an Elt fails to obtain for one value of v,
  -- don't search for it using any remaining value of v.
  (p',ss) = extendPossible v p s
  ces = S.map (runFindable d p' q) ss
  cesWithoutV = S.map (M.map $ S.map $ M.delete v) ces :: Set CondElts
    -- delete the dependency on v, so that reconciliation can work
  in reconcileCondElts cesWithoutV
    -- keep only results that obtain for every value of v

