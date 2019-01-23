data Source = Source  { sourceVar :: Var }
            | Source' { sourceVar :: Var
                      , dets :: (Set Var) }


sourceDets :: Source -> Set Var
sourceDets (Source v) = S.singleton v
sourceDets (Source' v dets) = dets

sourceRefs :: Source -> Set Var
sourceRefs (Source v) = S.singleton v
sourceRefs (Source' v dets) = S.insert v dets

extendPossible :: (Ord e, Show e) => Var -> Source -> Possible e -> Subst e
               -> Either String (Possible e, Set (Subst e))
extendPossible v src p s = do
  ce <- either (\msg -> Left $ "extendPossible: error in callee:\n" ++ msg)
    Right $ varPossibilities p s src
  let
    p' = case src of Source _ -> p
                     Source' _ _ -> M.insert v ce p
      -- TODO ? Does ce need this insertion into p? It is only a subset
      -- of a the CondElts already present, keyed by v' instead of v.
    s' = S.map (\k -> M.insert v k s) $ M.keysSet ce
  Right (p',s')

-- | `varPossibilities p s dv@(Var v dets)` is all values v0 dv can take
-- for which dv=v0 is an input into each of the dets.
-- dv should not be bound in s or p, and every det in dets should be.
-- If dets is null, then the CondElts returned
-- has no determinants, and the Subst is not used.

varPossibilities :: forall e. (Ord e, Show e)
                 => Possible e -> Subst e -> Source
                 -> Either String (CondElts e)
varPossibilities    p           _        (Source v) =
  maybe (Left $ keyErr "varPossibilities" v p) Right $ M.lookup v p

varPossibilities    p           s        (Source' v dets) = let
  (pRestricted :: Possible e) = M.map (f :: CondElts e -> CondElts e)
                              $ M.restrictKeys p vAndDets
    where -- A Subst in p that mentions a variable other than v or
          -- one of the dets is irrelevant, as is any such key of p.
      vAndDets = S.insert v dets
      f = M.map $ S.map $ flip M.restrictKeys vAndDets

  in case reconcileDetsAcrossVars pRestricted s dets of
    Left msg -> Left $ "varPossibilities: error in callee:\n" ++ msg
    Right (substs :: Set (Subst e)) -> let
      (possible :: Set e) = M.keysSet $ setSubstToCondElts v substs
      (mce_v :: Maybe (CondElts e)) = M.lookup v p
      in maybe (Left $ keyErr "varPossibilities" v p)
         (Right . flip M.restrictKeys possible) mce_v


-- | = functions that use the Types module

queryDets :: Query e sp -> Set Var
queryDets (QFind f)  = findDets f
queryDets (QTest t)  = testDets t
queryDets (QVTest t) = varTestDets t
queryDets _          = S.empty
