{-# LANGUAGE ViewPatterns #-}

module Query where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Subst
import Types


-- | `couldBind Q = Vs` <=> `Q` could depend on a binding of any var in `Vs`.
-- `willBind` would be a nice thing to define if it were possible, but
-- (without way more data and processing) it is not.
couldBind :: Query -> Set Var
couldBind (QFind _)      = S.empty
couldBind (QCond _)      = S.empty
couldBind (QOr  qs)      = S.unions                  $ map couldBind qs
couldBind (QAnd qs)      = S.unions                  $ map couldBind qs
couldBind (ForSome vf q) = S.insert (varFuncTarget vf) $   couldBind q
couldBind (ForAll  _  q) =                                 couldBind q

-- | Every `QAnd` must include something `findable`, and
-- every `QOr` must be nonempty and consist entirely of `findable` things.
findable :: Query -> Bool
findable (QFind _)          = True
findable (QCond _)          = False
findable (QAnd qs)          = or  $ map findable qs
findable (QOr     [])       = False
findable (QOr     qs@(_:_)) = and $ map findable qs
findable (ForSome vfs q)    = findable q
findable (ForAll  _   q)    = findable q

-- | A validity test.
disjointExistentials :: Query -> Bool
disjointExistentials (ForSome vf q)
  = not $ S.member (varFuncTarget vf) (couldBind q)
disjointExistentials (QAnd qs) = snd $ foldr f (S.empty, True) qs
  where f :: Query -> (Set Var, Bool) -> (Set Var, Bool)
        f _ (_, False) = (S.empty, False) -- short circuit (hence foldr)
        f q (vs, True) = if S.disjoint vs $ couldBind q
                         then (S.union vs $ couldBind q, True)
                         else (S.empty, False)
disjointExistentials _ = True

runFind :: Data -> Subst -> Find -> CondElts
runFind d s (Find find deps) =
  let found = find d s             :: Set Elt
      used = M.restrictKeys s deps :: Subst
  in M.fromSet (const $ S.singleton used) found

runCond :: Data -> Subst -> Cond -> Elt -> (Bool, Subst)
runCond d s (Cond test deps) e =
  let passes = test d s e          :: Bool
      used = M.restrictKeys s deps :: Subst
  in (passes, used)

runQuery :: Data -- TODO ? Is the `Var` argument needed here?
         -> Possible -- ^ how earlier `Var`s have been bound
         -> Subst  -- ^ earlier (higher, calling) quantifiers draw these
                   -- from the input `Possible`
         -> Var    -- ^ what we want to bind
         -> Query  -- ^ how we intend to bind it
         -> CondElts

runQuery d _ s _ (QFind f) = runFind d s f
runQuery _ _ _ _ (QCond _) =
  error "QCond cannot be run as a standalone Query."
--runQuery d r s v (ForSome (VarFunc v dets) q) =
--  let vPossibilities =
