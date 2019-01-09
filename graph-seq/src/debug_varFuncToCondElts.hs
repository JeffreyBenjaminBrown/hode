varFuncToCondElts :: Possible -> Subst -> VarFunc -> CondElts
varFuncToCondElts    p           s  vf@(VarFunc _ dets) = case null dets of
  True -> (M.!) p vf
  False -> let substs = varFuncSubsts p s vf :: Set Subst
               x = setSubstToCondElts vf substs :: CondElts
           in recordDependencies vf x :: CondElts

varFuncToCondElts r s_b2 aOf_b
 substs = varFuncSubsts r s_b2 aOf_b :: Set Subst
  = S.fl
    -- TODO : I think the problem is here. The "a" values here
    -- are unconditional, but `setSubstToCondElts` will be looking for
    -- "a" as a function of "b".
    [ M.fl [(VarFunc {varFuncTarget = "a", varFuncDets = fl []},2)]
    , M.fl [(VarFunc {varFuncTarget = "a", varFuncDets = fl []},3)
           ,(VarFunc {varFuncTarget = "x", varFuncDets = fl []},1)]]
 res = setSubstToCondElts aOf_b substs
     = M.empty :: CondElts
