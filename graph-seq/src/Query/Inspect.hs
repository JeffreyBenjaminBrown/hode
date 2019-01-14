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
-- a quantifier in a subquery, and that no two clauses of a `QAnd`
-- introduce the same variable.
--
-- (That second condition is stricter than necessary. For instance, the query
-- "Both (for all x, it is not x) and (for some x, it is a child of x)"
-- is meaningful: The x associated with the result could be from "for some",
-- while "for all" would generate no association. It ought to be that
-- a QAnd is valid as long as no two clauses could bind the same variable,
-- rather than quantify (`quantifies`) the same variable. But that's hard,
-- because a ForAll Test that runs after a ForSome Find would clobber
-- the ForSome's binding.)
--
-- TODO `disjointQuantifiers` should ensure that every quantifier
-- introducing a Var does not shadow that Var's source.

disjointQuantifiers :: Query -> Bool
disjointQuantifiers (ForSome vf _ q)
  = (not $ S.member vf $ quantifies q)          && disjointQuantifiers q
disjointQuantifiers (ForAll vf _ q)
  = (not $ S.member vf $ quantifies q)          && disjointQuantifiers q
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

checkInternalAndExternalVars :: Query -> (Set Var, Set Var)
checkInternalAndExternalVars q = f (S.empty,S.empty,True) q where
  merge :: (Set Var, Set Var)
        -> (Set Var, Set Var)
        -> (Set Var, Set Var)
  merge (s,s',b) (t,t',c) = (S.union s t, S.union s' t')
  f :: (Set Var, Set Var) -> Query -> (Set Var, Set Var)
  f ei (QOr  qs) = S.foldr merge $ map (f ei) qs
  f ei (QAnd qs) = S.foldr merge $ map (f ei) qs
--  f (e,i) (ForSome v s q) =
--  f (e,i) (ForAll  v s q) = 
  f (e,i) q = (S.union e notInInt, i) where
    notInInt = S.filter (not . flip S.member i) $ sourceRefs q

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, quantifies `v`.
-- And every `Query` quantifies whatever its subqueries quantifies.
quantifies :: Query -> Set Var
quantifies (QOr  qs)        = S.unions    $ map quantifies qs
quantifies (QAnd qs)        = S.unions    $ map quantifies qs
quantifies (ForSome vf _ q) = S.insert vf $     quantifies q
quantifies (ForAll  vf _ q) = S.insert vf $     quantifies q
quantifies _ = S.empty
