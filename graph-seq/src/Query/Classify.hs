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


validQuery :: Query -> Either String ()
validQuery q =
  case feasible'Junctions q of
  False -> Left $ "Infeasible junction in Query."
  True -> case disjointExistentials q of
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

  recursive j@(QAnd qs)   = simple j && and (map recursive qs)
  recursive j@(QOr  qs)   = simple j && and (map recursive qs)
  recursive (ForAll  _ q) =                      recursive q
  recursive (ForSome _ q) =                      recursive q
  recursive _             = True


-- | = Classifying Queries as findlike or testlike

findlike, testlike :: Query -> Bool

testlike = not . findlike

findlike (QFind _)          = True
findlike (QAnd qs)          = or  $ map findlike qs
findlike (QOr     qs@(_:_)) = and $ map findlike qs
findlike (ForSome _ q)      = findlike q
findlike (ForAll  _ q)      = findlike q
findlike _                  = False

-- | = Avoiding collisions between existentials

-- | `disjointExistentials` tests that no quantifier masks an earlier one,
-- and that no two clauses of a `QAnd` introduce the same variable.
--
-- (That second condition is stricter than necessary. For instance, the query
-- "Both (for all x, it is not x) and (for some x, it is a child of x)"
-- is meaningful: The x associated with the result would be from "for some",
-- while "for all" would generate no association. It ought to be that
-- a QAnd is valid as long as no two clauses `couldBind` the same variable,
-- rather than `introduces` the same variable. But that would take some
-- work, because a ForAll Test that runs after a ForSome Find would clobber
-- the ForSome's binding.)

disjointExistentials :: Query -> Bool
disjointExistentials (ForSome vf q)
  = not $ S.member vf $ introduces q
disjointExistentials (ForAll vf q)
  = not $ S.member vf $ introduces q
disjointExistentials (QAnd qs) = snd $ foldr f (S.empty, True) qs
  where f :: Query -> (Set Var, Bool) -> (Set Var, Bool)
        f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
        f q (vs, True) = if S.disjoint vs $ introduces q
                         then (S.union vs $ introduces q, True)
                         else (S.empty, False)
disjointExistentials _ = True

usesOnlyQuantifiedVariables :: Query -> Bool
usesOnlyQuantifiedVariables q = f S.empty q where
  f :: Set Var -> Query -> Bool
  f vs (QVarTest f)   = S.isSubsetOf (varTestDets  f) vs
  f vs (QFind f)      = S.isSubsetOf (findDets     f) vs
  f vs (QTest f)      = S.isSubsetOf (testDets     f) vs
  f vs (QAnd qs)      = and $ map (f vs) qs
  f vs (QOr qs)       = and $ map (f vs) qs
  f vs (ForAll v qs)  = f (S.insert v vs) qs
  f vs (ForSome v qs) = f (S.insert v vs) qs

-- | A `Query`, if it is `ForSome v _` or `ForAll v _`, introduces `v`.
-- And every `Query` introduces whatever its subqueries introduces.
introduces :: Query -> Set Var
introduces (QOr  qs)      = S.unions    $ map introduces qs
introduces (QAnd qs)      = S.unions    $ map introduces qs
introduces (ForSome vf q) = S.insert vf $     introduces q
introduces (ForAll  vf q) = S.insert vf $     introduces q
introduces _ = S.empty


-- | The only way a `Var` could be bound (outside of the binding Query)
-- is if it was introduced by a `ForSome`.
couldBind :: Query -> Set Var
couldBind (QOr  qs)      = S.unions    $ map couldBind qs
couldBind (QAnd qs)      = S.unions    $ map couldBind qs
couldBind (ForSome vf q) = S.insert vf $     couldBind q
couldBind (ForAll  _  q) =                   couldBind q
couldBind _              = S.empty
