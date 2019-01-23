  , TestLabel "testVarPossibilities" testVarPossibilities


testVarPossibilities = TestCase $ do
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      s_b1c1 = M.fromList [ (b,1), (c,1) ] :: Subst Int
      s_b2   = M.fromList [ (b,2)        ] :: Subst Int
      (pa :: Possible Int) = M.fromList [
        ( a, M.fromList [ (1, S.singleton mempty)
                        , (5, S.singleton $ M.singleton x 23) ] ) ]
  assertBool "0" $   varPossibilities pa M.empty (Source a)
    == Right ((M.!) pa a)
  assertBool "0.1" $ varPossibilities pa s_b1c1  (Source a)
    == Right ((M.!) pa a)
    -- the Subst s_b1c1 is ignored because the Source has no dets

  let (p :: Possible Int) = M.fromList
          [ ( a, M.fromList
              [ (1, S.singleton M.empty)
              , (2, S.singleton M.empty)
              , (3, S.singleton $ M.singleton x 13)
              , (4, S.singleton $ M.singleton x 14) ] )
          , ( b, M.fromList
              [ (1, S.fromList [ M.fromList [(a, 2), (x,0)       ]
                               , M.fromList [(a, 3), (x,1)       ]
                               , M.fromList [(a, 4), (x,1)       ] ] )
              , (2, S.fromList [ M.fromList [(a,2)               ]
                               , M.fromList [(a,3) , (x,1)       ] ] ) ] )
          , ( c, M.fromList
              [ (1, S.fromList [ M.fromList [(a, 2),       (y,3) ]
                               , M.fromList [(a, 4), (x,2)       ] ] )
              , (2, S.singleton M.empty) ] ) ]
      aOf_b  = Source' a $ S.singleton b
      aOf_bc = Source' a $ S.fromList [b, c]

  assertBool "1" $ varPossibilities p s_b2 aOf_b
    == Right (M.fromList [ (2, S.singleton M.empty )
                         , (3, S.singleton $ M.singleton x 13) ])
  assertBool "2" $ varPossibilities p s_b1c1 aOf_bc
    -- (b=1 using a=4) and (c=1 using a=4) imply different bindings for x;
    -- that's okay.
    == Right (M.fromList [ (2, S.singleton M.empty )
                         , (4, S.singleton $ M.singleton x 14) ])

