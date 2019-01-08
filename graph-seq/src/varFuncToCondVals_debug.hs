-- This just duplicates the code.
varFuncToCondElts :: Possible -> Subst -> VarFunc -> Maybe CondElts
varFuncToCondElts      p        s  vf@(VarFunc v dets) = case null dets of
  True -> Just $ (M.!) p v
  False ->
    let -- These two steps might be duplicating a lot of work.
      substs = varFuncSubsts p s vf                        :: Set Subst
      ces = S.map (restrictCondElts substs . (M.!) p) dets :: Set CondElts
    in reconcileCondElts ces

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
        , (2, S.singleton M.empty) ] ) ] :: Possible
aOf_b  = VarFunc a (S.fromList [b   ])
aOf_bc = VarFunc a (S.fromList [b, c])

-- Here are two worked examples,
-- relying on data temporarily top-leveled in Test.hs.
varFuncToCondElts r s_b1c1 aOf_bc =
  substs = varFuncSubsts r s_b1c1 aOf_bc :: Set Subst
         = S.fromList [fromList [(Var "a",2),(Var "x",0),(Var "y",3)]
                      ,fromList [(Var "a",3),(Var "y",3)]
                      ,fromList [(Var "a",4),(Var "x",1),(Var "y",2)]]
  -- TODO WTFIST
  ces = S.map (restrictCondElts substs . (M.!) r) (S.fromList [b,c])
      = sfl [mfl [(1,sfl [mfl [(Var "a",2),(Var "x",0),(Var "y",3)]
                         ,mfl [(Var "a",3),(Var "y",3)]
                         ,mfl [(Var "a",4),(Var "x",1),(Var "y",2)]])
                 ,(2,sfl [mfl [(Var "a",2),(Var "x",0),(Var "y",3)]])]
            ,mfl [(1,sfl [mfl [(Var "a",2),(Var "x",0),(Var "y",3)]
                         ,mfl [(Var "a",3),(Var "y",3)]
                         ,mfl [(Var "a",4),(Var "x",1),(Var "y",2)]])
                 ,(2,sfl [mfl [(Var "a",2),(Var "x",0),(Var "y",3)]
                         ,mfl [(Var "a",3),(Var "y",3)]
                         ,mfl [(Var "a",4),(Var "x",1),(Var "y",2)]])]]


varFuncToCondElts r s_b2 aOf_b =
  substs = varFuncSubsts r s_b2 aOf_b
         = fromList [fromList [(Var "a",2)]]
  ces = S.map (restrictCondElts substs . (M.!) r) (S.singleton b)
      = S.fromList [ M.fromList [ (1,M.fromList [S.fromList [(Var "a",2)
                                                            ,(Var "x",0)]])
                                , (2,M.fromList [S.fromList [(Var "a",2)]])]]
    :: Set CondElts
  res = reconcileCondElts ces :: Maybe CondElts
  Just (fromList [(1,fromList [fromList [(Var "a",2),(Var "x",0)]])
                 ,(2,fromList [fromList [(Var "a",2)]])])
