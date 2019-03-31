{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.TLookup where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import           Rslt.RLookup hiding (exprToAddr)
import qualified Rslt.Edit as R
import qualified Rslt.RLookup as R
import           Rslt.RTypes
import qualified Test.Rslt.RData as D


test_module_rslt_exprToAddr :: Test
test_module_rslt_exprToAddr = TestList [
  TestLabel "test_variety" test_variety
  , TestLabel "test_fills" test_fills
  , TestLabel "test_isIn" test_isIn
  , TestLabel "test_has" test_has
  , TestLabel "test_exprToAddr" test_exprToAddr
  , TestLabel "test_refExprToExpr" test_refExprToExpr
  ]

test_refExprToExpr :: Test
test_refExprToExpr = TestCase $ do
  assertBool "tplt" $ Right ( ExprTplt [ Phrase ""
                                       , Phrase "needs"
                                       , Phrase "" ] )
    == refExprToExpr D.rslt ( Tplt' [ 0, 3, 0 ] )

  assertBool "par" $ Right ( Par [ ( "You can't eat"
                                   , Phrase "oxygen" ) ]
                             "silly" )
    == refExprToExpr D.rslt ( Par' [("You can't eat", 2)] "silly" )

  assertBool "rel, recursive" $
    let ti = ExprTplt [ Phrase ""
                                                      , Phrase "needs"
                                                      , Phrase "" ]
    in Right ( ExprRel $ Rel [ Phrase "dog"
                             , ExprRel $ Rel [ Phrase "dog"
                                             , Phrase "oxygen" ]
                               ti ]
               ti )
    == refExprToExpr D.rslt ( Rel' $ Rel [1,5] 4 )

test_exprToAddr :: Test
test_exprToAddr = TestCase $ do
  assertBool "1" $ (R.exprToAddr D.rslt $ Addr 0)       == Right 0
  assertBool "2" $ isLeft
                 $ (R.exprToAddr D.rslt $ Addr $ -10000)
  assertBool "3" $ (R.exprToAddr D.rslt $ Phrase "needs") == Right 3
  assertBool "4" $ (R.exprToAddr D.rslt $ either (error "wut") id
                    $ refExprToExpr D.rslt $ Tplt' [0,3,0])  == Right 4
  assertBool "5" $ Right 4 ==
    R.exprToAddr D.rslt ( ExprTplt [ Addr 0
                                , Phrase "needs"
                                , Phrase ""] )

  assertBool "6" $ Right 5 ==
    R.exprToAddr D.rslt ( ExprRel $ Rel [ Addr 1
                                        , Phrase "oxygen"]
                          $ Addr 4 )
  assertBool "7" $ isLeft $
    R.exprToAddr D.rslt ( ExprRel $ Rel [ Addr 1
                                        , Phrase "oxygen"]
                          $ Addr 6 )

test_has :: Test
test_has = TestCase $ do
  assertBool "tplt" $ has D.rslt 4
    == Right ( M.fromList [ ( RoleMember 1, 0 )
                         , ( RoleMember 2, 3 )
                         , ( RoleMember 3, 0 ) ] )
  assertBool "rel" $ has D.rslt 5
    == Right ( M.fromList [ ( RoleMember 1, 1 )
                         , ( RoleMember 2, 2 )
                         , ( RoleTplt    , 4 ) ] )
  assertBool "par" $ has D.rslt 6
    == Right ( M.fromList [ ( RoleMember 1, 5 ) ] )
  assertBool "no content" $ has D.rslt 0 == Right M.empty
  assertBool "absent" $ isLeft $ has D.rslt 7

test_isIn :: Test
test_isIn = TestCase $ do
  assertBool "1" $ isIn D.rslt 0
    == Right ( S.fromList [ (RoleMember 1, 4)
                          , (RoleMember 3, 4) ] )
  assertBool "2" $ isIn D.rslt 4
    == Right ( S.fromList [ (RoleTplt, 5) ] )
  assertBool "3" $ let r' = either (error "wut") id
                            $ R.insertAt 7 (Phrase' "pizza") D.rslt
                   in isIn r' 7 == Right S.empty

test_fills :: Test
test_fills = TestCase $ do
  assertBool "1st in tplt"
    $ fills D.rslt (RoleMember 1, 4) == Right 0
  assertBool "2nd in tplt"
    $ fills D.rslt (RoleMember 2, 4) == Right 3
  assertBool "1st in rel"
    $ fills D.rslt (RoleMember 2, 5) == Right 2
  assertBool "2nd in rel"
    $ fills D.rslt (RoleMember 1, 5) == Right 1
  assertBool "nonexistent (3rd in binary)" $ isLeft
    $ fills D.rslt (RoleMember 3, 5)
  assertBool "tplt in rel"
    $ fills D.rslt (RoleTplt    , 5) == Right 4
  assertBool "nonexistent (tplt in par)" $ isLeft
    $ fills D.rslt (RoleTplt    , 6)
  assertBool "first in par"
    $ fills D.rslt (RoleMember 1, 6) == Right 5

test_variety :: Test
test_variety = TestCase $ do
  assertBool "1" $ variety D.rslt 3 == Right (PhraseCtr,0)
  assertBool "2" $ variety D.rslt 4 == Right (TpltCtr,2)
  assertBool "3" $ variety D.rslt 5 == Right (RelCtr,2)
  assertBool "4" $ variety D.rslt 6 == Right (ParCtr,1)
  assertBool "5" $ isLeft
                 $ variety D.rslt (-133)
