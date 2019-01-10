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
couldBind (QOr  qs)      = S.unions    $ map couldBind qs
couldBind (QAnd qs)      = S.unions    $ map couldBind qs
couldBind (ForSome vf q) = S.insert vf $     couldBind q
couldBind (ForAll  _  q) =                   couldBind q

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
  = not $ S.member vf $ couldBind q
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

runQuery :: Data
         -> Possible -- ^ how the `Program`'s earlier `Var`s have been bound
         -> Query
         -> Subst  -- ^ earlier (higher, calling) quantifiers draw these
                   -- from the input `Possible`
         -> CondElts

runQuery d _ (QFind f) s = runFind d s f
runQuery _ _ (QCond _) _ =
  error "QCond cannot be run as a standalone Query."

runQuery d p (ForSome v@(Var _ dets) q) s = let
  vPossible = varToCondElts p s v :: CondElts
  p' = if null dets then p else M.insert v vPossible p
  substs = S.map (\k -> M.insert v k s) $ M.keysSet vPossible :: Set Subst
  ces = S.map (runQuery d p' q) substs :: Set CondElts
  in M.unionsWith S.union ces

runQuery d p (ForAll v@(Var _ dets) q) s = let
  -- TODO (#speed|#major) Once an elt fails to obtain for one value of v,
  -- don't try it for any of the remaining values of v.
  vPossible = varToCondElts p s v :: CondElts
  p' = if null dets then p else M.insert v vPossible p
  substs = S.map (\k -> M.insert v k s) $ M.keysSet vPossible :: Set Subst
  ces = S.map (runQuery d p' q) substs :: Set CondElts
  cesWithoutV = S.map (M.map $ S.map $ M.delete v) ces :: Set CondElts
    -- delete the dependency on v, so that reconciliation can work
  in maybe M.empty id $ reconcileCondElts cesWithoutV :: CondElts
        -- keep only results that obtain for every value of v
