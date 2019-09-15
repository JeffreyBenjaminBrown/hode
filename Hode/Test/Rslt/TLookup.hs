{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TLookup where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import           Hode.Rslt.RLookup hiding (exprToAddr)
import qualified Hode.Rslt.Edit.Initial as R
import qualified Hode.Rslt.RLookup      as R
import           Hode.Rslt.RTypes
import qualified Hode.Test.Rslt.RData   as D


test_module_rslt_exprToAddr :: Test
test_module_rslt_exprToAddr = TestList [
  TestLabel "test_variety" test_variety
  , TestLabel "test_fills" test_fills
  , TestLabel "test_isIn" test_isIn
  , TestLabel "test_has" test_has
  , TestLabel "test_exprToAddr" test_exprToAddr
  , TestLabel "test_refExprToExpr" test_refExprToExpr
  , TestLabel "test_unAddr" test_unAddr
  ]

test_unAddr :: Test
test_unAddr = TestCase $ do
  assertBool "" $ unAddr D.r0 (Addr 0) == Right (Phrase "")
  assertBool "" $
    unAddrRec D.r0 ( ExprRel $ Rel
                     [ Addr 0, Phrase "dog" ] $ Phrase "" ) ==
    Right (ExprRel $ Rel [ Phrase "", Phrase "dog" ] $ Phrase "")

test_refExprToExpr :: Test
test_refExprToExpr = TestCase $ do
  assertBool "tplt" $ Right ( ExprTplt $ Tplt Nothing
                                              [Phrase "needs"]
                                              Nothing )
    == refExprToExpr D.rslt ( Tplt' $ Tplt Nothing [3] Nothing )

  assertBool "rel, recursive" $
    let ti = ExprTplt $ Tplt Nothing [Phrase "needs"] Nothing
    in Right ( ExprRel $ Rel [ Phrase "dog"
                             , ExprRel $ Rel [ Phrase "dog"
                                             , Phrase "oxygen" ]
                               ti ]
               ti )
    == refExprToExpr D.rslt ( Rel' $ Rel [1,5] 4 )

test_exprToAddr :: Test
test_exprToAddr = TestCase $ do
  assertBool "1" $ (R.exprToAddr D.rslt $ Addr 0)
    == Right 0
  assertBool "2" $ isLeft
                 $ (R.exprToAddr D.rslt $ Addr $ -10000)
  assertBool "3" $ (R.exprToAddr D.rslt $ Phrase "needs")
    == Right 3
  assertBool "4" $ (R.exprToAddr D.rslt $ either (error "wut") id
                     $ refExprToExpr D.rslt
                     $ Tplt' (Tplt Nothing [3] Nothing))
    == Right 4
  assertBool "5" $ Right 4 ==
    R.exprToAddr D.rslt ( ExprTplt $ Tplt (Just $ Addr 0)
                                          [Phrase "needs"]
                                          (Just $ Phrase "") )

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
  assertBool "no content" $ has D.rslt 0 == Right M.empty
  assertBool "absent" $ isLeft $ has D.rslt 7

test_isIn :: Test
test_isIn = TestCase $ do
  assertBool "1" $ isIn D.rslt 0
    == Right ( S.fromList [ (RoleMember 1, 4)
                          , (RoleMember 3, 4) ] )
  assertBool "2" $ isIn D.rslt 4
    == Right ( S.fromList [ (RoleTplt, 5)
                          , (RoleTplt, 6) ] )
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

test_variety :: Test
test_variety = TestCase $ do
  assertBool "1" $ variety D.rslt 3 == Right (PhraseCtr,0)
  assertBool "2" $ variety D.rslt 4 == Right (TpltCtr,2)
  assertBool "3" $ variety D.rslt 5 == Right (RelCtr,2)
  assertBool "5" $ isLeft
                 $ variety D.rslt (-133)
