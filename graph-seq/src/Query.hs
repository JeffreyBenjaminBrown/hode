module Query where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


compatibleSubsts :: Subst -> Subst -> Maybe Subst
compatibleSubsts s t = S.foldl f (Just M.empty) allKeys where
  allKeys = S.union (M.keysSet s) (M.keysSet t) :: Set Var
  f :: Maybe Subst -> Var -> Maybe Subst
  f Nothing _ = Nothing -- short-circuit (roughly)
  f (Just acc) v =
    if        S.member v (M.keysSet s)
    then if   S.member v (M.keysSet t)
         then if (M.!) t v /= (M.!) s v
              then Nothing
              else Just $ M.insert v ((M.!) s v) acc
         else      Just $ M.insert v ((M.!) s v) acc
    else           Just $ M.insert v ((M.!) t v) acc

-- | `dets` are `Var`s that depended on `v`'s earlier calculation
-- for their own. They are bound in the `Subst`, so they determine what
-- `v` could be.
lookupVarFunc :: Result -> Subst -> VarFunc -> ConditionedValues
lookupVarFunc      r        s   (VarFunc v dets) =
  let vCandidates :: Var -> Set Subst
      vCandidates det = (M.!) couldBindTo bound where
        bound = (M.!) s det :: Elt
        couldBindTo = (M.!) r det :: ConditionedValues
  in mempty

-- | `couldBind Q = Vs` <=> `Q` could depend on a binding of any var in `Vs`.
-- `willBind` would be a nice thing to define if it were possible, but
-- (without way more data and processing) it is not.
couldBind :: Query -> Set Var
couldBind (QFind _)      = S.empty
couldBind (QCond _)      = S.empty
couldBind (QOr  qs)      = S.unions                  $ map couldBind qs
couldBind (QAnd qs)      = S.unions                  $ map couldBind qs
couldBind (ForSome vf q) = S.insert (varFuncName vf) $     couldBind q
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
  = not $ S.member (varFuncName vf) (couldBind q)
disjointExistentials (QAnd qs) = snd $ foldl f (S.empty, True) qs
  where f :: (Set Var, Bool) -> Query -> (Set Var, Bool)
        f (_, False) _ = (S.empty, False) -- short circuit (roughly)
        f (vs, True) q = if S.disjoint vs $ couldBind q
                         then (S.union vs $ couldBind q, True)
                         else (S.empty, False)
disjointExistentials _ = True

runFind :: Data -> Subst -> Find -> ConditionedValues
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
         -> Result -- ^ how earlier `Var`s have been bound
         -> Subst  -- ^ these are drawn from the input `Result`
         -> Var    -- ^ what we want to bind
         -> Query  -- ^ how we want to bind it
         -> ConditionedValues

runQuery d _ s _ (QFind f) = runFind d s f
runQuery _ _ _ _ (QCond _) =
  error "QCond cannot be run as a standalone Query."
-- runQuery d r s v (Forall vf q) =
