-- This just duplicates the code.
varFuncToCondElts :: Possible -> Subst -> VarFunc -> Maybe CondElts
varFuncToCondElts      r        s  vf@(VarFunc v dets) = case null dets of
  True -> Just $ (M.!) r v
  False -> let
    substs = varFuncSubsts r s vf :: Set Subst
    ces = S.map (restrictCondVals substs . (M.!) r) dets :: Set CondElts
      -- each member of ces is an M.singleton
    sss = S.map (snd . M.findMin) ces :: Set (Set Subst)
      -- For each map in ces, this let us return its only value
      -- (and effectively discard the key).
    in setSetSubstToCondElts v sss

-- Here are variable definitions good for testing it.
(a,b,c,x,y) = (Var "a",Var "b",Var "c",Var "x",Var "y")
vf_a    = VarFunc a (S.empty)
s_b1c1 = M.fromList [ (b,1), (c,1) ] :: Subst
s_b2   = M.fromList [ (b,2)        ] :: Subst
ra = M.fromList [
  ( a, M.fromList [ (1, S.singleton mempty)
                  , (5, S.singleton $ M.singleton x 23) ] ) ] :: Possible
r = M.fromList
    [ ( a, M.fromList
        [ (1, S.singleton $ error "never used")
        , (2, error "doesn't matter") ] )
    , ( b, M.fromList
        [ (1, S.fromList [ M.fromList [(a, 2), (x,0)       ]
                         , M.fromList [(a, 3)              ]
                         , M.fromList [(a, 4), (x,1)       ] ] )
        , (2, S.fromList [M.fromList [(a,2)] ] ) ] )
    , ( c, M.fromList
        [ (1, S.fromList [ M.fromList [(a, 2),       (y,3) ]
                         , M.fromList [(a, 3),       (y,3) ]
                         , M.fromList [(a, 4), (y,2)       ] ] )
        , (2, error "never used, doesn't matter") ] ) ] :: Possible
aOf_b  = VarFunc a (S.fromList [b   ])
aOf_bc = VarFunc a (S.fromList [b, c])

-- Here is a worked example,
-- relying on data temporarily top-leveled in Test.hs.
varFuncToCondElts r s_b2 aOf_b =
  substs = varFuncSubsts r s_b2 aOf_b
         = fromList [fromList [(Var "a",2)]]
  -- TODO :z The map in ces is not a singleton!
  ces = S.map (restrictCondVals substs . (M.!) r) (S.singleton b)
      = S.fromList [ M.fromList [ (1,M.fromList [S.fromList [(Var "a",2)
                                                            ,(Var "x",0)]])
                                , (2,M.fromList [S.fromList [(Var "a",2)]])]]
    :: Set CondElts
  sss = S.map (snd . M.findMin) ces :: Set (Set Subst)
      = S.fromList [ S.fromList [ M.fromList [(Var "a",2)
                                             ,(Var "x",0)]]]

