{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hash.TConvert where

import           Data.Either
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit

import Hash.Convert
import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes


test_module_hash_convert :: Test
test_module_hash_convert = TestList [
    TestLabel "test_simplifyPExpr" test_simplifyPExpr
  , TestLabel "test_pRelToHExpr" test_pRelToHExpr
  , TestLabel "test_pExprToHExpr" test_pExprToHExpr
  ]

test_pRelToHExpr :: Test
test_pRelToHExpr = TestCase $ do
  assertBool "1" $ isLeft $ pRelToHExpr Absent
  assertBool "2" $ pRelToHExpr ( Closed
                                 [ pnrWord "a", pnrWord "b" ]
                                 [ "is" ] )
    == Right ( HMap $ M.fromList
               [ ( RoleTplt, HExpr $ Tplt [ Word "is" ] )
               , ( RoleMember 1, HExpr $ Word "a" )
               , ( RoleMember 2, HExpr $ Word "b" ) ] )
  assertBool "3" $ let meh = error "irrelevant"
    in pRelToHExpr ( Open meh [ pnrWord "a", pnrWord "b" ] [ "is" ] )
    == pRelToHExpr ( Closed   [ pnrWord "a", pnrWord "b" ] [ "is" ] )

  assertBool "4" $ pRelToHExpr ( Closed
                                 [ pnrWord "a"
                                 , Closed [ pnrWord "c", pnrWord "d" ] [ "to"]
                                 , pnrWord "b" ]
                                 [ "is", "because" ] )
    == Right
    ( HMap $ M.fromList
      [ ( RoleTplt    , HExpr $ Tplt [ Word "is", Word "because" ] )
      , ( RoleMember 1, HExpr $ Word "a" )
      , ( RoleMember 2, HMap $ M.fromList
                        [ ( RoleTplt    , HExpr $ Tplt [ Word "to" ] )
                        , ( RoleMember 1, HExpr $ Word "c" )
                        , ( RoleMember 2, HExpr $ Word "d" ) ] )
      , ( RoleMember 3, HExpr $ Word "b" ) ] )

test_pExprToHExpr :: Test
test_pExprToHExpr = TestCase $ do
  assertBool "1" $ ( pExprToHExpr
                     ( PEval $ PMap $ M.fromList
                       [ ( RoleTplt, PExpr $ Tplt [ Word "is" ] )
                       , ( RoleMember 1, It Nothing ) ] ) )
    == Right ( HEval
               ( HMap $ M.fromList
                 [ ( RoleTplt
                   , HExpr ( Tplt [ Word "is" ] ) ) ] )
               [ [ RoleMember 1 ] ] )

  assertBool "2" $
    pExprToHExpr ( PEval $ PRel $ Open (error "irrelevant")
      [ PNonRel $ PMap $ M.fromList
        [ ( RoleMember 1, PExpr $ Word "bugs" )
        , ( RoleMember 2, It Nothing ) ]
      , PNonRel $ PExpr $ Word "sassafras"
      , PNonRel $ Any ]
      [ "enjoy", "because" ]
    ) == Right ( HEval
                 ( HMap $ M.fromList
                   [ ( RoleTplt, ( HExpr $ Tplt
                                   [ Word "enjoy", Word "because" ] ) )
                   , ( RoleMember 1, HMap $ M.singleton
                                     ( RoleMember 1 )
                                     $ HExpr $ Word "bugs" ),
                     ( RoleMember 2, HExpr $ Word "sassafras" ) ] )
                 [ [ RoleMember 1, RoleMember 2 ] ] )

test_simplifyPExpr :: Test
test_simplifyPExpr = TestCase $ do
  assertBool "1" $
    simplifyPExpr ( PAnd [ PAnd [ PAnd [ PExpr $ Word "a"
                                       , PExpr $ Word "b" ]
                         , PAnd [ PAnd [ PExpr $ Word "c"
                                       , PExpr $ Word "d" ] ] ] ] )
    == PAnd (map (PExpr . Word) ["a","b","c","d"] )
