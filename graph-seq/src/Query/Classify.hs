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

-- | = findable and testable
-- Every `QAnd` must include something `findable`, and every
-- `QOr` must be nonempty and consist entirely of `findable` queries.

findable, testable :: Query -> Bool
findable (QFind _)          = True
findable (QTest _)          = False
findable (QAnd qs)          = or  $ map findable qs
findable (QOr     [])       = False
findable (QOr     qs@(_:_)) = and $ map findable qs
findable (ForSome _ q)      = findable q
findable (ForAll  _ q)      = findable q

testable = not . findable

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

-- | A `Query` is only valid if no quantifier masks an earlier one,
-- and if no two clauses of a `QAnd` could bind the same variable.
okExistentials :: Query -> Bool
okExistentials (ForSome vf q)
  = not $ S.member vf $ introduces q
okExistentials (ForAll vf q)
  = not $ S.member vf $ introduces q
okExistentials (QAnd qs) = snd $ foldr f (S.empty, True) qs
  where f :: Query -> (Set Var, Bool) -> (Set Var, Bool)
        f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
        f q (vs, True) = if S.disjoint vs $ couldBind q
                         then (S.union vs $ couldBind q, True)
                         else (S.empty, False)
okExistentials _ = True

