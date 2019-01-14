{-# LANGUAGE ViewPatterns #-}
module Query.Classify where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Subst
import Types
import Util


validQuery :: Query -> Either String ()
validQuery q =
  case feasible'Junctions q of
  False -> Left $ "Infeasible junction in Query."
  True -> case disjointQuantifiers q of
    False -> Left $ "Existentials not disjoint in Query."
    True -> case usesOnlyQuantifiedVariables q of
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


-- | `disjointQuantifiers` tests that no quantifier masks an earlier one,
-- and that no two clauses of a `QAnd` introduce the same variable.
--
-- (That second condition is stricter than necessary. For instance, the query
-- "Both (for all x, it is not x) and (for some x, it is a child of x)"
-- is meaningful: The x associated with the result could be from "for some",
-- while "for all" would generate no association. It ought to be that
-- a QAnd is valid as long as no two clauses could bind the same variable,
-- rather than quantify (`quantifies`) the same variable. But that's hard,
-- because a ForAll Test that runs after a ForSome Find would clobber
-- the ForSome's binding.)

disjointQuantifiers :: Query -> Bool
disjointQuantifiers (ForSome vf _ q)
  = (not $ S.member vf $ quantifies q)          && disjointQuantifiers q
disjointQuantifiers (ForAll vf _ q)
  = (not $ S.member vf $ quantifies q)          && disjointQuantifiers q
disjointQuantifiers (QOr qs) =           and $ map disjointQuantifiers qs
disjointQuantifiers (QAnd qs) =
  (snd $ foldr f (S.empty, True) qs) && (and $ map disjointQuantifiers qs)
  where f :: Query -> (Set Var, Bool) -> (Set Var, Bool)
        f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
        f q (vs, True) = if S.disjoint vs $ quantifies q
                         then (S.union vs $ quantifies q, True)
                         else (S.empty, False)
disjointQuantifiers _ = True

-- | A Var can only be used (by a Test, VarTest or Find)
-- if it has first been introduced by a ForAll or ForSome.
usesOnlyQuantifiedVariables :: Query -> Bool
usesOnlyQuantifiedVariables q = f S.empty q where
  f :: Set Var -> Query -> Bool
  f vs (ForAll  v (Source s) qs) = f (S.insert v vs) qs
  f vs (ForSome v (Source s) qs) = f (S.insert v vs) qs
  f vs (ForAll  v (Source' s dets) qs) =
    if not $ S.isSubsetOf (S.insert s dets) vs then False
    else f (S.insert v vs) qs
  f vs (ForSome v (Source' s dets) qs) =
    if not $ S.isSubsetOf (S.insert s dets) vs then False
    else f (S.insert v vs) qs
  f vs (QAnd qs)        = and $ map (f vs) qs
  f vs (QOr qs)         = and $ map (f vs) qs
  f vs _                = S.isSubsetOf (queryDets q) vs

-- | Note that `sourcesReferTo` checks Sources but not Source's.
-- Source's are checked by `usesOnlyQuantifiedVariables`.
sourcesReferTo :: Query -> Set Var
sourcesReferTo (QOr  qs)                = S.unions $ map sourcesReferTo qs
sourcesReferTo (QAnd qs)                = S.unions $ map sourcesReferTo qs
sourcesReferTo (ForSome _ (Source v) q) = S.insert v $   sourcesReferTo q
sourcesReferTo (ForAll  _ (Source v) q) = S.insert v $   sourcesReferTo q
sourcesReferTo q                        = queryDets q

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, quantifies `v`.
-- And every `Query` quantifies whatever its subqueries quantifies.
quantifies :: Query -> Set Var
quantifies (QOr  qs)        = S.unions    $ map quantifies qs
quantifies (QAnd qs)        = S.unions    $ map quantifies qs
quantifies (ForSome vf _ q) = S.insert vf $     quantifies q
quantifies (ForAll  vf _ q) = S.insert vf $     quantifies q
quantifies _ = S.empty
