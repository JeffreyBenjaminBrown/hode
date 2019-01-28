module Test.TXslt where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Space.Xslt
import           Space.Rslt.RTypes
import qualified Test.Rslt.XData as D


test_module_xslt = TestList [
  TestLabel "test_variety" test_variety
  , TestLabel "test_isIn1" test_isIn1
  , TestLabel "test_isIn" test_isIn
  , TestLabel "test_imgLookup" test_imgLookup
  ]

test_imgLookup = TestCase $ do
  assertBool "1" $ (xImgLookup D.xslt $ ImgOfAddr 0)              == Just 0
  assertBool "2" $ (xImgLookup D.xslt $ ImgOfAddr $ -10000)       == Nothing
  assertBool "3" $ (xImgLookup D.xslt $ ImgOfExpr $ Word "needs") == Just 3
  assertBool "4" $ (xImgLookup D.xslt $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  assertBool "5" $ Just 4 ==
    xImgLookup D.xslt ( ImgOfTplt [ ImgOfAddr 0
                                  , ImgOfExpr $ Word "needs"
                                  , ImgOfExpr $ Word ""] )

  assertBool "6" $ Just 5 ==
    xImgLookup D.xslt ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 4 )
  assertBool "7" $ Nothing ==
    xImgLookup D.xslt ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 6 )

test_isIn = TestCase $ do
  assertBool "1" $ M.lookup 0 (xIsIn D.xslt)
    == Just ( S.fromList [ (RoleMember 1, 4)
                         , (RoleMember 3, 4) ] )
  assertBool "1" $ M.lookup 4 (xIsIn D.xslt)
    == Just ( S.fromList [ (RoleTplt, 5) ] )

test_isIn1 = TestCase $ do
  assertBool "1" $ isIn1 D.xslt (RoleMember 1, 4) == Just 0
  assertBool "2" $ isIn1 D.xslt (RoleMember 2, 4) == Just 3
  assertBool "3" $ isIn1 D.xslt (RoleMember 2, 5) == Just 2
  assertBool "4" $ isIn1 D.xslt (RoleMember 1, 5) == Just 1
  assertBool "5" $ isIn1 D.xslt (RoleTplt, 5)     == Just 4
  assertBool "6" $ isIn1 D.xslt (RoleTplt, 6)     == Nothing

test_variety = TestCase $ do
  assertBool "1" $ M.lookup 3      (xVarieties D.xslt) == Just (Word',0)
  assertBool "2" $ M.lookup 4      (xVarieties D.xslt) == Just (Tplt',2)
  assertBool "3" $ M.lookup 5      (xVarieties D.xslt) == Just (Rel',2)
  assertBool "4" $ M.lookup 6      (xVarieties D.xslt) == Just (Par',1)
  assertBool "5" $ M.lookup (-133) (xVarieties D.xslt) == Nothing
