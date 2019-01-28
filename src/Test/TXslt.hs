module Test.TRslt where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Space.Rslt
import           Space.Rslt.RTypes
import qualified Test.Rslt.RData as D


test_module_rslt = TestList [
  TestLabel "test_variety" test_variety
  , TestLabel "test_isIn1" test_isIn1
  , TestLabel "test_isIn" test_isIn
  , TestLabel "test_imgLookup" test_imgLookup
  ]

test_imgLookup = TestCase $ do
  assertBool "1" $ (xImgLookup D.rslt $ ImgOfAddr 0)              == Just 0
  assertBool "2" $ (xImgLookup D.rslt $ ImgOfAddr $ -10000)       == Nothing
  assertBool "3" $ (xImgLookup D.rslt $ ImgOfExpr $ Word "needs") == Just 3
  assertBool "4" $ (xImgLookup D.rslt $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  assertBool "5" $ Just 4 ==
    xImgLookup D.rslt ( ImgOfTplt [ ImgOfAddr 0
                                  , ImgOfExpr $ Word "needs"
                                  , ImgOfExpr $ Word ""] )

  assertBool "6" $ Just 5 ==
    xImgLookup D.rslt ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 4 )
  assertBool "7" $ Nothing ==
    xImgLookup D.rslt ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 6 )

test_isIn = TestCase $ do
  assertBool "1" $ M.lookup 0 (__rIsIn D.rslt)
    == Just ( S.fromList [ (RoleMember 1, 4)
                         , (RoleMember 3, 4) ] )
  assertBool "1" $ M.lookup 4 (__rIsIn D.rslt)
    == Just ( S.fromList [ (RoleTplt, 5) ] )

test_isIn1 = TestCase $ do
  assertBool "1" $ isIn1 D.rslt (RoleMember 1, 4) == Just 0
  assertBool "2" $ isIn1 D.rslt (RoleMember 2, 4) == Just 3
  assertBool "3" $ isIn1 D.rslt (RoleMember 2, 5) == Just 2
  assertBool "4" $ isIn1 D.rslt (RoleMember 1, 5) == Just 1
  assertBool "5" $ isIn1 D.rslt (RoleTplt, 5)     == Just 4
  assertBool "6" $ isIn1 D.rslt (RoleTplt, 6)     == Nothing

test_variety = TestCase $ do
  assertBool "1" $ M.lookup 3      (_varieties D.rslt) == Just (Word',0)
  assertBool "2" $ M.lookup 4      (_varieties D.rslt) == Just (Tplt',2)
  assertBool "3" $ M.lookup 5      (_varieties D.rslt) == Just (Rel',2)
  assertBool "4" $ M.lookup 6      (_varieties D.rslt) == Just (Par',1)
  assertBool "5" $ M.lookup (-133) (_varieties D.rslt) == Nothing
