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

test_disjointExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      [x,x1,x2,y,y1,y2] = ["x","x1","x2","y","y1","y2"]
      qx  = QQuant $ ForSome  "x" x qf
      qy  = QQuant $ ForSome  "y" y qf
      qx1 = QQuant $ ForSome "x1" x qf
      qy1 = QQuant $ ForSome "y1" y qf
      qxy = QJunct $ Or [qx1,qy1]
  assertBool "-1" $ disjointQuantifiers (QQuant $ ForSome x y qx)
    == False
  assertBool "0" $ disjointQuantifiers  (QQuant $ ForSome x x qy)
    == False
  assertBool "1" $ disjointQuantifiers  (QQuant $ ForSome x2 y qx1)
    == True
  assertBool "2" $ disjointQuantifiers  (QQuant $ ForSome y x qx1)
    == True
  assertBool "3" $ disjointQuantifiers qx               == False
  assertBool "3.5" $ disjointQuantifiers qx1            == True
  assertBool "3.7" $ disjointQuantifiers qxy            == True
  assertBool "4" $ disjointQuantifiers (QJunct $ And [qx1,qxy]) == False

test_internalAndExternalVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      -- These queries are named for their internal and external variables.
      c_de, c_a :: QIGI
      c_de = QQuant $ ForSome c d -- d was: (Source' d $ S.singleton a)
        $ QFind $ findParents $ Right e
      c_a = QQuant $ ForSome c a -- a was: (Source' a $ S.singleton g)
        $ QFind $ findParents $ Right c
  assertBool "5" $ internalAndExternalVars
    (QJunct $ Or [ c_de, c_a ] :: QIGI)
    == (S.singleton c, S.fromList [a,d,e,g] )
  assertBool "4" $ internalAndExternalVars c_de
    == (S.singleton c, S.fromList [a,d,e])
  assertBool "3" $ internalAndExternalVars c_a
    == (S.singleton c, S.fromList [a,g])
  assertBool "2" $ internalAndExternalVars
    (QFind $ findParents $ Right c :: QIGI)
    == (S.empty, S.singleton c)
  assertBool "1" $ internalAndExternalVars
    ( QQuant $ ForAll a b $ QJunct $ Or [ c_de, c_a ] )
    == ( S.fromList [a,c ], S.fromList [b,d,e,g ] )
