module Test.TRslt where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Space.Rslt
import Space.Rslt.Index
import Space.Rslt.Index.Positions
import Space.Rslt.Index.ImgLookup
import Space.Rslt.Search
import qualified Test.Rslt.Data as D
import Util


test_module_rslt = TestList [
  TestLabel "test_invertPositions" test_invertPositions
  , TestLabel "test_variety" test_variety
  , TestLabel "test_holdsPosition" test_holdsPosition
  , TestLabel "test_imgLookup" test_imgLookup
  , TestLabel "test_checkDb" test_checkDb
  , TestLabel "test_search" test_search
  , TestLabel "test_searchSubst" test_searchSubst
  , TestLabel "test_joinSubsts" test_joinSubsts
  , TestLabel "test_negativeRQuery" test_negativeRQuery
  ]

test_negativeRQuery = TestCase $ do
  assertBool "1" $ ( negativeRQuery D.index
                     ( RQNot $ RQUnion [ RQImg $ ImgOfAddr 0
                                       , RQImg $ ImgOfAddr 1 ] )
                     1 ) == False
  assertBool "2" $ ( negativeRQuery D.index
                     ( RQNot $ RQUnion [ RQImg $ ImgOfAddr 0
                                       , RQImg $ ImgOfAddr 1 ] )
                     2 ) == True
  assertBool "3" $ True  == negativeRQuery D.index (RRQVariety Tplt') 4
  assertBool "4" $ False == negativeRQuery D.index (RRQVariety Tplt') 5

test_joinSubsts = TestCase $ do
  assertBool "1" $ join2Substs M.empty M.empty == Right M.empty
  assertBool "2" $ join2Substs ( M.fromList [(Var "a",0), (Var "b",1)])
              ( M.fromList [(Var "b",1), (Var "a",0)])
    == (Right $ M.fromList [(Var "b",1), (Var "a",0)])
  assertBool "3" $ join2Substs ( M.fromList [(Var "a",0), (Var "b",1)])
    ( M.fromList [(Var "b",1)] )
    == (Right $ M.fromList [(Var "b",1), (Var "a",0)])
  assertBool "4" $ joinSubsts [] == Right M.empty
  assertBool "5" $ joinSubsts [ M.singleton (Var "a") 0
                              , M.singleton (Var "b") 1
                              , M.singleton (Var "c") 2 ]
             == Right (M.fromList [(Var "a",0), (Var "b",1), (Var "c",2)])
  assertBool "6" $ joinSubsts [ M.singleton (Var "a") 0
                              , M.empty
                              , M.singleton (Var "a") 2 ]
    == Left ()

test_searchSubst = TestCase $ do
  assertBool "1" $ searchSubst D.index
    (RQImg $ ImgOfExpr $ Word "") == M.fromList [(0, M.empty)]
  assertBool "2" $ searchSubst D.index
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

test_checkDb = TestCase $ do
  assertBool "1" $ M.toList (relsWithoutMatchingTplts   D.badFiles D.index)
    == [(1001, Rel [1,2] 5), (1002, Rel [1, 2] $ -1000)]
  assertBool "2" $ M.toList (collectionsWithAbsentAddrs D.badFiles D.index)
    == [(1002, [-1000])]

test_imgLookup = TestCase $ do
  assertBool "1" $ (imgLookup D.files $ ImgOfAddr 0)              == Just 0
  assertBool "2" $ (imgLookup D.files $ ImgOfAddr $ -10000)       == Nothing
  assertBool "3" $ (imgLookup D.files $ ImgOfExpr $ Word "needs") == Just 3
  assertBool "4" $ (imgLookup D.files $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  assertBool "5" $ Just 4 ==
    imgLookup D.files ( ImgOfTplt [ ImgOfAddr 0
                                  , ImgOfExpr $ Word "needs"
                                  , ImgOfExpr $ Word ""] )

  assertBool "6" $ Just 5 ==
    imgLookup D.files ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 4 )
  assertBool "7" $ Nothing ==
    imgLookup D.files ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 6 )

test_holdsPosition = TestCase $ do
  assertBool "1" $ holdsPosition D.index (RoleMember 1, 4) == Just 0
  assertBool "2" $ holdsPosition D.index (RoleMember 2, 4) == Just 3
  assertBool "3" $ holdsPosition D.index (RoleMember 2, 5) == Just 2
  assertBool "4" $ holdsPosition D.index (RoleMember 1, 5) == Just 1
  assertBool "5" $ holdsPosition D.index (RoleTplt, 5) == Just 4
  assertBool "6" $ holdsPosition D.index (RoleTplt, 6) == Nothing

test_variety = TestCase $ do
  assertBool "1" $ variety D.index 3 == Just (Word',0)
  assertBool "2" $ variety D.index 4 == Just (Tplt',2)
  assertBool "3" $ variety D.index 5 == Just (Rel',2)
  assertBool "4" $ variety D.index 6 == Just (Par',1)
  assertBool "5" $ variety D.index (-133) == Nothing

test_invertPositions = TestCase $ do
  let ips = positionsHeldByAll [ (1,  [ (RoleMember 1, 11 )
                                      , (RoleMember 2, 22 ) ] )
                               , (11, [ (RoleMember 1, 1  )
                                      , (RoleMember 2, 22 ) ] )
                               , (3,  [ (RoleMember 1, 1  ) ] )
                               ]
  assertBool "1" $ ips == M.fromList [(1,  S.fromList [(RoleMember 1,3  )
                                                      ,(RoleMember 1,11 )])
                                     ,(11, S.fromList [(RoleMember 1,1  )])
                                     ,(22, S.fromList [(RoleMember 2,1  )
                                                      ,(RoleMember 2,11 )])]
