{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hash.TConvert where

import           Data.Either
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit

import Hash.Convert
import Hash.EitherExpr
import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes


test_module_hash_convert = TestList [
    TestLabel "test_pRelToHExpr" test_pRelToHExpr
  , TestLabel "test_pNonRelToHExpr" test_pNonRelToHExpr
  ]

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

test_pNonRelToHExpr = TestCase $ do
  assertBool "1" $ ( pNonRelToHExpr
                     ( PEval $ PMap $ M.fromList
                       [ ( RoleTplt, PExpr $ Tplt [ Word "is" ] )
                       , ( RoleMember 1, It Nothing ) ] ) )
    == Right ( HEval
               ( HMap $ M.fromList
                 [ ( RoleTplt
                   , HExpr ( Tplt [ Word "is" ] ) ) ] )
               [ [ RoleMember 1 ] ] )

  assertBool "2" $ x == error "voogle"

x = pNonRelToHExpr
    ( PEval $ PRel $ Open (error "irrelevant")
      [ PNonRel $ PMap $ M.fromList
        [ ( RoleMember 1, PExpr $ Word "bugs" )
        , ( RoleMember 2, It Nothing ) ]
      , PNonRel $ PExpr $ Word "sassafras"
      , PNonRel $ Any ]
      [ "enjoy", "because" ]
    )

