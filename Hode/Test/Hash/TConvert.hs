{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.TConvert where

import           Data.Either
import qualified Data.Map       as M
import           Test.HUnit

import Hode.Hash.Convert
import Hode.Hash.HTypes
import Hode.Hash.HUtil
import Hode.Rslt.Index
import Hode.Rslt.RTypes


test_module_hash_convert :: Test
test_module_hash_convert = TestList [
    TestLabel "test_simplifyPExpr" test_simplifyPExpr
  , TestLabel "test_pRelToHExpr" test_pRelToHExpr
  , TestLabel "test_pExprToHExpr" test_pExprToHExpr
  , TestLabel "test_pathsToIts" test_pathsToIts
  ]

test_pathsToIts :: Test
test_pathsToIts = TestCase $ do
  assertBool "" $ pathsToIts_sub_pRel
    ( Closed
      [ Closed
        [ PNonRel $ PExpr $ Phrase "b"
        , PNonRel $ It $ Just $ PExpr $ Phrase "a" ]
        [ "has" ]
      , PNonRel $ PExpr $ Phrase "c" ]
      [ "is" ] )
    == Right [[RoleMember 1, RoleMember 2]]
  assertBool "" $ pathsToIts_sub_pRel
    ( Closed
      [ PNonRel $ It $ Just $ PExpr $ Phrase "a"
      , PNonRel $ It $ Just $ PExpr $ Phrase "b" ]
      [ "is" ] )
    == Right [[RoleMember 1],[RoleMember 2]]
  assertBool "next: pathsToIts_pExpr" False

test_pRelToHExpr :: Test
test_pRelToHExpr = TestCase $ do
  let r = mkRslt mempty
  assertBool "1" $ isLeft $ pRelToHExpr r Absent
  assertBool "2" $ pRelToHExpr r ( Closed
                                   [ pnrPhrase "a", pnrPhrase "b" ]
                                   [ "is" ] )
    == Right ( HMap $ M.fromList
               [ ( RoleTplt, HExpr $ ExprTplt $ map Phrase [ "", "is", "" ] )
               , ( RoleMember 1, HExpr $ Phrase "a" )
               , ( RoleMember 2, HExpr $ Phrase "b" ) ] )
  assertBool "3" $ let meh = error "irrelevant"
    in pRelToHExpr r ( Open meh [ pnrPhrase "a", pnrPhrase "b" ] [ "is" ] )
    == pRelToHExpr r ( Closed   [ pnrPhrase "a", pnrPhrase "b" ] [ "is" ] )

  assertBool "4" $ pRelToHExpr r
    ( Closed
      [ pnrPhrase "a"
      , Closed [ pnrPhrase "c", pnrPhrase "d" ] [ "to"]
      , pnrPhrase "b" ]
      [ "is", "because" ] )
    == Right
    ( HMap $ M.fromList
      [ ( RoleTplt
        , HExpr $ ExprTplt $ map Phrase ["", "is", "because", "" ] )
      , ( RoleMember 1, HExpr $ Phrase "a" )
      , ( RoleMember 2, HMap $ M.fromList
                        [ ( RoleTplt    , ( HExpr $ ExprTplt $ map Phrase
                                            [ "","to", "" ] ) )
                        , ( RoleMember 1, HExpr $ Phrase "c" )
                        , ( RoleMember 2, HExpr $ Phrase "d" ) ] )
      , ( RoleMember 3, HExpr $ Phrase "b" ) ] )

test_pExprToHExpr :: Test
test_pExprToHExpr = TestCase $ do
  let r = mkRslt mempty
  assertBool "1" $ ( pExprToHExpr r
                     ( PEval $ PMap $ M.fromList
                       [ ( RoleTplt, PExpr $ ExprTplt [ Phrase "is" ] )
                       , ( RoleMember 1, It Nothing ) ] ) )
    == Right ( HEval
               ( HMap $ M.fromList
                 [ ( RoleTplt
                   , HExpr ( ExprTplt [ Phrase "is" ] ) ) ] )
               [ [ RoleMember 1 ] ] )

  assertBool "2" $
    pExprToHExpr r ( PEval $ PRel $ Open (error "irrelevant")
      [ PNonRel $ PMap $ M.fromList
        [ ( RoleMember 1, PExpr $ Phrase "bugs" )
        , ( RoleMember 2, It Nothing ) ]
      , PNonRel $ PExpr $ Phrase "sassafras"
      , PNonRel $ Any ]
      [ "enjoy", "because" ]
    ) == Right ( HEval
                 ( HMap $ M.fromList
                   [ ( RoleTplt, ( HExpr $ ExprTplt $ map Phrase
                                   ["", "enjoy", "because", "" ] ) )
                   , ( RoleMember 1, HMap $ M.singleton
                                     ( RoleMember 1 )
                                     $ HExpr $ Phrase "bugs" ),
                     ( RoleMember 2, HExpr $ Phrase "sassafras" ) ] )
                 [ [ RoleMember 1, RoleMember 2 ] ] )


test_simplifyPExpr :: Test
test_simplifyPExpr = TestCase $ do
  assertBool "1" $
    simplifyPExpr ( PAnd [ PAnd [ PAnd [ PExpr $ Phrase "a"
                                       , PExpr $ Phrase "b" ]
                         , PAnd [ PAnd [ PExpr $ Phrase "c"
                                       , PExpr $ Phrase "d" ] ] ] ] )
    == PAnd (map (PExpr . Phrase) ["a","b","c","d"] )
