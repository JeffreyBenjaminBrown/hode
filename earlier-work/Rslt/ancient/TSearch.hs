  , TestLabel "test_search" test_search
  , TestLabel "test_searchRSubst" test_searchRSubst
  , TestLabel "test_joinRSubsts" test_joinRSubsts
  , TestLabel "test_negativeRQuery" test_negativeRQuery


test_negativeRQuery = TestCase $ do
  assertBool "1" $ ( negativeRQuery D.index
                     ( RQNot $ RQUnion [ RQImg $ ImgOfAddr 0
                                       , RQImg $ ImgOfAddr 1 ] )
                     1 ) == False
  assertBool "2" $ ( negativeRQuery D.index
                     ( RQNot $ RQUnion [ RQImg $ ImgOfAddr 0
                                       , RQImg $ ImgOfAddr 1 ] )
                     2 ) == True
  assertBool "3" $ True  == negativeRQuery D.index (RQVariety Tplt') 4
  assertBool "4" $ False == negativeRQuery D.index (RQVariety Tplt') 5

test_joinRSubsts = TestCase $ do
  assertBool "1" $ join2RSubsts M.empty M.empty == Right M.empty
  assertBool "2" $ join2RSubsts ( M.fromList [(Var "a",0), (Var "b",1)])
              ( M.fromList [(Var "b",1), (Var "a",0)])
    == (Right $ M.fromList [(Var "b",1), (Var "a",0)])
  assertBool "3" $ join2RSubsts ( M.fromList [(Var "a",0), (Var "b",1)])
    ( M.fromList [(Var "b",1)] )
    == (Right $ M.fromList [(Var "b",1), (Var "a",0)])
  assertBool "4" $ joinRSubsts [] == Right M.empty
  assertBool "5" $ joinRSubsts [ M.singleton (Var "a") 0
                              , M.singleton (Var "b") 1
                              , M.singleton (Var "c") 2 ]
             == Right (M.fromList [(Var "a",0), (Var "b",1), (Var "c",2)])
  assertBool "6" $ joinRSubsts [ M.singleton (Var "a") 0
                              , M.empty
                              , M.singleton (Var "a") 2 ]
    == Left ()

test_searchRSubst = TestCase $ do
  assertBool "1" $ searchRSubst D.index
    (RQImg $ ImgOfExpr $ Word "") == M.fromList [(0, M.empty)]
  assertBool "2" $ searchRSubst D.index
    (RQImg $ ImgOfAddr 5) == M.fromList [(5, M.empty)]

test_search = TestCase $ do
  assertBool "1" $ S.fromList [4] ==
    search D.index (RQHasInRole (RoleMember 1) $ RQImg $ ImgOfExpr $ Word "")
  assertBool "2" $ S.fromList [4] ==
    search D.index (RQHasInRole (RoleMember 3) $ RQImg $ ImgOfExpr $ Word "")
  assertBool "3" $ S.empty ==
    search D.index (RQHasInRole (RoleMember 2) $ RQImg $ ImgOfExpr $ Word "")

  assertBool "4" $ S.fromList [4] ==
    search D.index (RQHasInRoles [ (RoleMember 1, RQImg $ ImgOfAddr 0)
                                 , (RoleMember 2, RQImg $ ImgOfAddr 3)
                                 ] )
  assertBool "5" $ S.empty ==
    search D.index (RQHasInRoles [ (RoleMember 1, RQImg $ ImgOfAddr 0)
                                 , (RoleMember 2, RQImg $ ImgOfAddr 2)
                                 ] )
