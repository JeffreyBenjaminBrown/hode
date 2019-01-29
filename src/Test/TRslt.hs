{-# LANGUAGE ScopedTypeVariables #-}

module Test.TRslt where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Data.Rslt hiding (insert, lookup)
import qualified Data.Rslt as R
import           Data.Rslt.RTypes
import qualified Test.Rslt.RData as D


test_module_rslt = TestList [
  TestLabel "test_variety" test_variety
  , TestLabel "test_isIn1" test_isIn1
  , TestLabel "test_isIn" test_isIn
  , TestLabel "test_has" test_has
  , TestLabel "test_lookup" test_lookup
  , TestLabel "test_insert" test_insert
  , TestLabel "test_deleteUnusedExpr" test_deleteUnusedExpr
  ]

test_deleteUnusedExpr = TestCase $ do
  -- deleteUnusedExpr :: Addr -> Rslt -> Either String Rslt
  let (e_without_6 :: Exprs) = M.delete 6 D.exprs
      (without_6   :: Rslt)  = mkRslt e_without_6
      (without_5_6 :: Rslt)  = either (error "wut") id $ deleteUnusedExpr 5 without_6
  assertBool "1" $ isLeft $ deleteUnusedExpr 5 D.rslt
  assertBool "2" $ exprAt without_5_6 5 == Nothing
  assertBool "in progress" False

test_insert = TestCase $ do
  let r2 = R.insert 7 (Rel [1,1] 4) D.rslt
  assertBool "1" $ isIn r2 4 == Just (S.fromList [ (RoleTplt    , 7     )
                                                 , (RoleTplt    , 5     ) ] )
  assertBool "2" $ isIn r2 1 == Just (S.fromList [ (RoleMember 1, 7     )
                                                 , (RoleMember 2, 7     )
                                                 , (RoleMember 1, 5     ) ] )
  assertBool "3" $ has r2 7 == Just ( M.fromList [ (RoleMember 1, 1     )
                                                 , (RoleMember 2, 1     )
                                                 , (RoleTplt    , 4     ) ] )
  assertBool "4" $ map (has D.rslt) [1..6] == map (has r2) [1..6]
  assertBool "5" $      has D.rslt  7      == Nothing

test_lookup = TestCase $ do
  assertBool "1" $ (R.lookup D.rslt $ ImgOfAddr 0)              == Just 0
  assertBool "2" $ (R.lookup D.rslt $ ImgOfAddr $ -10000)       == Nothing
  assertBool "3" $ (R.lookup D.rslt $ ImgOfExpr $ Word "needs") == Just 3
  assertBool "4" $ (R.lookup D.rslt $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  assertBool "5" $ Just 4 ==
    R.lookup D.rslt ( ImgOfTplt [ ImgOfAddr 0
                                , ImgOfExpr $ Word "needs"
                                , ImgOfExpr $ Word ""] )

  assertBool "6" $ Just 5 ==
    R.lookup D.rslt ( ImgOfRel [ ImgOfAddr 1
                               , ImgOfExpr $ Word "oxygen"]
                      $ ImgOfAddr 4 )
  assertBool "7" $ Nothing ==
    R.lookup D.rslt ( ImgOfRel [ ImgOfAddr 1
                               , ImgOfExpr $ Word "oxygen"]
                      $ ImgOfAddr 6 )

test_has = TestCase $ do
  assertBool "tplt" $ has D.rslt 4 == Just ( M.fromList [ ( RoleMember 1, 0 )
                                                        , ( RoleMember 2, 3 )
                                                        , ( RoleMember 3, 0 ) ] )
  assertBool "rel" $ has D.rslt 5 == Just ( M.fromList [ ( RoleMember 1, 1 )
                                                       , ( RoleMember 2, 2 )
                                                       , ( RoleTplt    , 4 ) ] )
  assertBool "par" $ has D.rslt 6 == Just ( M.fromList [ ( RoleMember 1, 5 ) ] )
  assertBool "no content" $ has D.rslt 0 == Just M.empty
  assertBool "absent" $ has D.rslt 7 == Nothing

test_isIn = TestCase $ do
  assertBool "1" $ M.lookup 0 (_isIn D.rslt)
    == Just ( S.fromList [ (RoleMember 1, 4)
                         , (RoleMember 3, 4) ] )
  assertBool "2" $ M.lookup 4 (_isIn D.rslt)
    == Just ( S.fromList [ (RoleTplt, 5) ] )
  assertBool "3" $ isIn (R.insert 7 (Word "pizza") D.rslt) 7
    == Just S.empty

test_isIn1 = TestCase $ do
  assertBool "1st in tplt"                 $ isIn1 D.rslt (RoleMember 1, 4) == Just 0
  assertBool "2nd in tplt"                 $ isIn1 D.rslt (RoleMember 2, 4) == Just 3
  assertBool "1st in rel"                  $ isIn1 D.rslt (RoleMember 2, 5) == Just 2
  assertBool "2nd in rel"                  $ isIn1 D.rslt (RoleMember 1, 5) == Just 1
  assertBool "nonexistent (3rd in binary)" $ isIn1 D.rslt (RoleMember 3, 5) == Nothing
  assertBool "tplt in rel"                 $ isIn1 D.rslt (RoleTplt    , 5) == Just 4
  assertBool "nonexistent (tplt in par)"   $ isIn1 D.rslt (RoleTplt    , 6) == Nothing
  assertBool "first in par"                $ isIn1 D.rslt (RoleMember 1, 6) == Just 5

test_variety = TestCase $ do
  assertBool "1" $ M.lookup 3      (_variety D.rslt) == Just (Word',0)
  assertBool "2" $ M.lookup 4      (_variety D.rslt) == Just (Tplt',2)
  assertBool "3" $ M.lookup 5      (_variety D.rslt) == Just (Rel',2)
  assertBool "4" $ M.lookup 6      (_variety D.rslt) == Just (Par',1)
  assertBool "5" $ M.lookup (-133) (_variety D.rslt) == Nothing
