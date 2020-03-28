module Test.TRslt where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Data.Rslt
import           Data.Rslt.Types
import           Data.Rslt.Index
import           Data.Rslt.Index.Positions
import           Data.Rslt.Index.ImgLookup
import qualified Test.Rslt.RData as D



  , TestLabel "test_variety" test_variety
  , TestLabel "test_holdsPosition" test_holdsPosition
  , TestLabel "test_imgLookup" test_imgLookup

test_imgLookup = TestCase $ do
  assertBool "1" $ (imgLookup D.exprs $ ImgOfAddr 0)              == Just 0
  assertBool "2" $ (imgLookup D.exprs $ ImgOfAddr $ -10000)       == Nothing
  assertBool "3" $ (imgLookup D.exprs $ ImgOfExpr $ Word "needs") == Just 3
  assertBool "4" $ (imgLookup D.exprs $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  assertBool "5" $ Just 4 ==
    imgLookup D.exprs ( ImgOfTplt [ ImgOfAddr 0
                                  , ImgOfExpr $ Word "needs"
                                  , ImgOfExpr $ Word ""] )

  assertBool "6" $ Just 5 ==
    imgLookup D.exprs ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 4 )
  assertBool "7" $ Nothing ==
    imgLookup D.exprs ( ImgOfRel [ ImgOfAddr 1
                                 , ImgOfExpr $ Word "oxygen"]
                        $ ImgOfAddr 6 )

test_holdsPosition = TestCase $ do
  assertBool "1" $ holdsPosition D.rslt (RoleMember 1, 4) == Just 0
  assertBool "2" $ holdsPosition D.rslt (RoleMember 2, 4) == Just 3
  assertBool "3" $ holdsPosition D.rslt (RoleMember 2, 5) == Just 2
  assertBool "4" $ holdsPosition D.rslt (RoleMember 1, 5) == Just 1
  assertBool "5" $ holdsPosition D.rslt (RoleTplt, 5) == Just 4
  assertBool "6" $ holdsPosition D.rslt (RoleTplt, 6) == Nothing

test_variety = TestCase $ do
  assertBool "1" $ variety D.rslt 3 == Just (Word',0)
  assertBool "2" $ variety D.rslt 4 == Just (Tplt',2)
  assertBool "3" $ variety D.rslt 5 == Just (Rel',2)
  assertBool "4" $ variety D.rslt 6 == Just (Par',1)
  assertBool "5" $ variety D.rslt (-133) == Nothing
