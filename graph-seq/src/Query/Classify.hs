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


-- | == Testing that a Query is valid.

-- | = findlike and testlike
-- Every `QAnd` must include something `findlike`, and every
-- `QOr` must be nonempty and consist entirely of `findlike` queries.

findlike, testlike :: Query -> Bool

testlike = not . findlike

findlike (QFind _)          = True
findlike (QTest _)          = False
findlike (QAnd qs)          = or  $ map findlike qs
findlike (QOr     [])       = False
findlike (QOr     qs@(_:_)) = and $ map findlike qs
findlike (ForSome _ q)      = findlike q
findlike (ForAll  _ q)      = findlike q


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
