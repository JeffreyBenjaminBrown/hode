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

testlike = not . findlike

findlike, testlike :: Query -> Bool
findlike (QFind _)          = True
findlike (QTest _)          = False
findlike (QAnd qs)          = or  $ map findlike qs
findlike (QOr     [])       = False
findlike (QOr     qs@(_:_)) = and $ map findlike qs
findlike (ForSome _ q)      = findlike q
findlike (ForAll  _ q)      = findlike q


-- | = Avoiding collisions between existentials

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
