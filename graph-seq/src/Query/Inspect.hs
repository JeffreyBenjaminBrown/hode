{-# LANGUAGE ViewPatterns #-}
module Query.Inspect where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


validQuery :: Query -> Either String ()
validQuery q =
  case feasible'Junctions q of
  False -> Left $ "Infeasible junction in Query."
  True -> case disjointQuantifiers q of
    False -> Left $ "Existentials not disjoint in Query."
    True -> case findsAndTestsOnlyQuantifiedVars q of
      False -> Left $ "Variable referred to before quantification."
      True -> Right ()

feasible'Junctions :: Query -> Bool
feasible'Junctions = recursive where
  simple, recursive :: Query -> Bool

  simple (QAnd qs) = not $ null $ filter findlike qs
  simple (QOr qs)  = and $           map findlike qs
  simple _         = True

  recursive j@(QAnd qs)     = simple j && and (map recursive qs)
  recursive j@(QOr  qs)     = simple j && and (map recursive qs)
  recursive (ForAll  _ _ q) =                      recursive q
  recursive (ForSome _ _ q) =                      recursive q
  recursive _               = True


-- | = Classifying Queries as findlike or testlike

findlike, testlike :: Query -> Bool

testlike = not . findlike

findlike (QFind _)          = True
findlike (QAnd qs)          = or  $ map findlike qs
findlike (QOr     qs@(_:_)) = and $ map findlike qs
findlike (ForSome _ _ q)    = findlike q
findlike (ForAll  _ _ q)    = findlike q
findlike _                  = False


-- | = Ensuring the Vars used in a Query make sense

-- | `disjointQuantifiers` tests that no quantifier is masked by
-- a quantifier in a subquery, that no quantifier masks part of its Source,
-- and that no two clauses of a `QAnd` introduce the same variable.
--
-- TODO ? Those conditions are stricter than necessary, but hard to relax.

disjointQuantifiers :: Query -> Bool
disjointQuantifiers (ForSome v s q)
  = (not $ S.member v $ S.union (quantifies q) $ sourceRefs s)
                                                && disjointQuantifiers q
disjointQuantifiers (ForAll v s q)
  = (not $ S.member v $ S.union (quantifies q) $ sourceRefs s)
                                                && disjointQuantifiers q
disjointQuantifiers (QOr qs) =           and $ map disjointQuantifiers qs
disjointQuantifiers (QAnd qs) =
  (snd $ foldr f (S.empty, True) qs) && (and $ map disjointQuantifiers qs)
  where -- `f` verifies that no Var is quantified in two clauses of the QAnd
    f :: Query -> (Set Var, Bool) -> (Set Var, Bool)
    f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
    f q (vs, True) = if S.disjoint vs $ quantifies q
                     then (S.union vs $ quantifies q, True)
                     else (S.empty, False)
disjointQuantifiers _ = True

-- | A Var can only be used (by a Test, VarTest or Find)
-- if it has first been introduced by a ForAll or ForSome.
findsAndTestsOnlyQuantifiedVars :: Query -> Bool
findsAndTestsOnlyQuantifiedVars q = f S.empty q where
  f :: Set Var -> Query -> Bool
  f vs (ForAll  v _ qs) = f (S.insert v vs) qs
  f vs (ForSome v _ qs) = f (S.insert v vs) qs
  f vs (QAnd qs)        = and $ map (f vs) qs
  f vs (QOr qs)         = and $ map (f vs) qs
  f vs _                = S.isSubsetOf (queryDets q) vs

-- | `internalAndExternalVars` checks Sources and quantifiers.
--
-- TODO: `internalAndExternalVars` should consider both Sources and Source's.
-- The Var that a quantifier binds to is always internal. If a Source or a
-- Source' refers to
-- a previously quantified var, then the Var that it binds to is internal.
-- If it does not, it refers to something external, and should go into the
-- result.

internalAndExternalVars :: Query -> (Set Var, Set Var)
internalAndExternalVars q = f (S.empty,S.empty) q where
  merge :: (Set Var, Set Var) -> (Set Var, Set Var) -> (Set Var, Set Var)
  merge (s,s') (t,t') = (S.union s t, S.union s' t')

  f :: (Set Var, Set Var) -> Query -> (Set Var, Set Var)
  f ie (QOr  qs) = S.foldr merge (S.empty, S.empty)
    $ S.fromList $ map (f ie) qs
  f ie (QAnd qs) = S.foldr merge (S.empty, S.empty)
    $ S.fromList $ map (f ie) qs
  f (i,e) (ForSome v s q) = f (S.insert v i, S.union e notInInt) q
    where notInInt = S.filter (not . flip S.member i) $ sourceRefs s
  f (i,e) (ForAll  v s q) = f (S.insert v i, S.union e notInInt) q
    where notInInt = S.filter (not . flip S.member i) $ sourceRefs s
  f (i,e) q = (i, S.union e notInInt)
    where notInInt = S.filter (not . flip S.member i) $ queryDets q

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, quantifies `v`.
-- And every `Query` quantifies whatever its subqueries quantifies.
quantifies :: Query -> Set Var
quantifies (QOr  qs)        = S.unions    $ map quantifies qs
quantifies (QAnd qs)        = S.unions    $ map quantifies qs
quantifies (ForSome v _ q) = S.insert v $     quantifies q
quantifies (ForAll  v _ q) = S.insert v $     quantifies q
quantifies _ = S.empty
