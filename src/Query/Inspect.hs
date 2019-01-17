{-# LANGUAGE ScopedTypeVariables #-}
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


validProgram :: [(Var,Query e sp)] -> Either String ()
validProgram vqs = let
  wholeProgramTest, individualQueryTests :: Either String ()
  wholeProgramTest     = noPrematureReference vqs
  individualQueryTests = foldl f (Right ()) vqs
    where
      f :: Either String () -> (Var, Query e sp) -> Either String ()
      f e@(Left _) _     = e
      f (Right ()) (_,q) = validQuery q
  in case wholeProgramTest of
  e@(Left _) -> e
  Right () -> individualQueryTests

noPrematureReference :: forall e sp. [(Var,Query e sp)] -> Either String ()
noPrematureReference vqs = case null bad of
  True -> Right ()
  False -> Left $ "validProgram: variables " ++ show bad
           ++ " used before being defined.\n"
  where
  (_,bad) = foldl f (S.empty, S.empty) vqs :: (Set Var, Set Var)
  f :: (Set Var, Set Var) -> (Var, Query e sp) -> (Set Var, Set Var)
  f (defined,bad) (v,q) =
    -- "defined" are variables defined by previous Queries.
    -- "bad" are variables used before being defined.
    let (_,ext) = internalAndExternalVars q
        moreBad = S.filter (not . flip S.member defined) ext
    in (S.insert v defined, S.union bad moreBad)

validQuery :: Query e sp -> Either String ()
validQuery q =
  case feasible'Junctions q of
  False -> Left $ "Infeasible junction in Query."
  True -> case disjointQuantifiers q of
    False -> Left $ "Existentials not disjoint in Query."
    True -> case findsAndTestsOnlyQuantifiedVars q of
      False -> Left $ "Variable referred to before quantification."
      True -> Right ()

feasible'Junctions :: Query e sp -> Bool
feasible'Junctions = recursive where
  simple, recursive :: Query e sp -> Bool

  simple (QAnd qs) = not $ null $ filter findlike qs
  simple (QOr qs)  = and $           map findlike qs
  simple _         = True

  recursive j@(QAnd qs) = simple j && and (map recursive qs)
  recursive j@(QOr  qs) = simple j && and (map recursive qs)
  recursive (QQuant w)  =                      recursive $ goal w
  recursive _           = True


-- | = Classifying Queries as findlike or testlike

findlike, testlike :: Query e sp -> Bool

testlike = not . findlike

findlike (QFind _)          = True
findlike (QAnd qs)          = or  $ map findlike qs
findlike (QOr     qs@(_:_)) = and $ map findlike qs
findlike (QQuant w)         =           findlike $ goal w
findlike _                  = False


-- | = Ensuring the Vars used in a Query make sense

-- | `disjointQuantifiers` tests that no quantifier is masked by
-- a quantifier in a subquery, that no quantifier masks part of its Source,
-- and that no two clauses of a `QAnd` introduce the same variable.
--
-- TODO ? Those conditions are stricter than necessary, but hard to relax.

disjointQuantifiers :: Query e sp -> Bool
disjointQuantifiers (QQuant w) = let (v,s,q) = (name w, source w, goal w)
  in (not $ S.member v $ S.union (quantifies q) $ sourceRefs s)
                                                && disjointQuantifiers q
disjointQuantifiers (QOr qs) =           and $ map disjointQuantifiers qs
disjointQuantifiers (QAnd qs) =
  (snd $ foldr f (S.empty, True) qs) && (and $ map disjointQuantifiers qs)
  where -- `f` verifies that no Var is quantified in two clauses of the QAnd
    f :: Query e sp -> (Set Var, Bool) -> (Set Var, Bool)
    f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
    f q (vs, True) = if S.disjoint vs $ quantifies q
                     then (S.union vs $ quantifies q, True)
                     else (S.empty, False)
disjointQuantifiers _ = True

-- | A Var can only be used (by a Test, VarTest or Find)
-- if it has first been introduced by a ForAll or ForSome.
findsAndTestsOnlyQuantifiedVars :: Query e sp -> Bool
findsAndTestsOnlyQuantifiedVars q = f S.empty q where
  f :: Set Var -> Query e sp -> Bool
  f vs (QQuant w) = f (S.insert (name w) vs) $ goal w
  f vs (QAnd qs)  = and $ map (f vs) qs
  f vs (QOr qs)   = and $ map (f vs) qs
  f vs _          = S.isSubsetOf (queryDets q) vs

-- | `internalAndExternalVars` checks Sources and quantifiers.

internalAndExternalVars :: Query e sp -> (Set Var, Set Var)
internalAndExternalVars q = f (S.empty,S.empty) q where
  merge :: (Set Var, Set Var) -> (Set Var, Set Var) -> (Set Var, Set Var)
  merge (s,s') (t,t') = (S.union s t, S.union s' t')

  f :: (Set Var, Set Var) -> Query e sp -> (Set Var, Set Var)
  f ie (QOr  qs) = S.foldr merge (S.empty, S.empty)
    $ S.fromList $ map (f ie) qs
  f ie (QAnd qs) = S.foldr merge (S.empty, S.empty)
    $ S.fromList $ map (f ie) qs
  f (i,e) (QQuant w) = f (S.insert v i, S.union e notInInt) q
    where (v,s,q) = (name w, source w, goal w)
          notInInt = S.filter (not . flip S.member i) $ sourceRefs s
  f (i,e) q = (i, S.union e notInInt)
    where notInInt = S.filter (not . flip S.member i) $ queryDets q

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, quantifies `v`.
-- And every `Query` quantifies whatever its subqueries quantifies.
quantifies :: Query e sp -> Set Var
quantifies (QOr  qs)  = S.unions          $ map quantifies qs
quantifies (QAnd qs)  = S.unions          $ map quantifies qs
quantifies (QQuant w) = S.insert (name w) $     quantifies (goal w)
quantifies _          = S.empty
