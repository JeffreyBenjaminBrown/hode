{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.TConvert where

import           Data.Either
import qualified Data.Map       as M
import           Test.HUnit
import           Text.Megaparsec (parse)

import Hode.Hash.Convert
import Hode.Hash.HParse
import Hode.Hash.HTypes
import Hode.Hash.HUtil
import Hode.Rslt.Index
import Hode.Rslt.RTypes


test_module_hash_convert :: Test
test_module_hash_convert = TestList [
    TestLabel "test_simplifyPExpr" test_simplifyPExpr
  , TestLabel "test_pRelToHExpr" test_pRelToHExpr
  , TestLabel "test_pExprToHExpr" test_pExprToHExpr
  , TestLabel "test_pathsToIts" test_pathsToIts_sub_pRel
  , TestLabel "test_pathsToIts_pExpr" test_pathsToIts_pExpr
  ]

test_HEval :: Test
test_HEval = TestCase $ do
  let a_hash_b = HMap
        ( M.fromList
          [ ( RoleTplt,
              HExpr ( ExprTplt [Phrase "",Phrase "",Phrase ""])),
            ( RoleMember 1, HExpr $ Phrase "a"),
            ( RoleMember 2, HExpr $ Phrase "b")])
  assertBool "/eval (/it=/h a # b) # c" $
    ( pExprToHExpr (mkRslt mempty) <$>
      parse pPExpr "" "/eval (/it=/h a # b) # c" ) ==
    ( Right $ Right $ HEval
      ( HMap ( M.fromList
               [ ( RoleTplt,
                   HExpr (ExprTplt [Phrase "",Phrase "",Phrase ""]) )
               , ( RoleMember 1, a_hash_b),
                 ( RoleMember 2, HExpr $ Phrase "c") ]))
      [[RoleMember 1]] )
  assertBool "/eval c # (/it=/h a # b)" $
    ( pExprToHExpr (mkRslt mempty) <$>
      parse pPExpr "" "/eval c # (/it=/h a # b)" ) ==
    ( Right $ Right $ HEval
      ( HMap ( M.fromList
               [ ( RoleTplt,
                   HExpr (ExprTplt [Phrase "",Phrase "",Phrase ""]) )
               , ( RoleMember 2, a_hash_b),
                 ( RoleMember 1, HExpr $ Phrase "c") ]))
      [[RoleMember 1]] )

test_pathsToIts_pExpr :: Test
test_pathsToIts_pExpr = TestCase $ do
  assertBool "It's okay if there's no It to find." $
    pathsToIts_pExpr ( PMap $ M.fromList
                       [ ( RoleMember 1, PExpr $ Phrase "moo" )
                       , ( RoleMember 2, PExpr $ Phrase "quack" )
                       ] )
    == Right []

  assertBool "It is the first member" $
    pathsToIts_pExpr
    ( PMap $ M.fromList
      [ ( RoleMember 1, It $ Just $ PExpr $ Phrase "moo" )
      , ( RoleMember 2, PExpr $ Phrase "quack" )
      ] )
    == Right [[RoleMember 1]]

  assertBool "It is both members, so there are two paths" $
    pathsToIts_pExpr
    ( PMap $ M.fromList
      [ ( RoleMember 1, It $ Just $ PExpr $ Phrase "moo" )
      , ( RoleMember 2, It $ Just $ PExpr $ Phrase "quack" )
      ] )
    == Right [[RoleMember 1],[RoleMember 2]]

  assertBool "The first member is a PRel, and its second member is It." $
    pathsToIts_pExpr
    ( PMap $ M.fromList
      [ ( RoleMember 1
        , PRel $ Closed [ PNonRel $ PExpr $ Phrase "quack"
                        , PNonRel $ It $ Just $ PExpr $ Phrase "moo" ]
          ["","resembles",""] )
      , ( RoleMember 2, PExpr $ Phrase "quack" ) ] )
    == Right [[ RoleMember 1, RoleMember 2 ]]

test_pathsToIts_sub_pRel :: Test
test_pathsToIts_sub_pRel = TestCase $ do
  assertBool "one length-2 path" $ pathsToIts_sub_pRel
    ( Closed
      [ Closed
        [ PNonRel $ PExpr $ Phrase "b"
        , PNonRel $ It $ Just $ PExpr $ Phrase "a" ]
        [ "has" ]
      , PNonRel $ PExpr $ Phrase "c" ]
      [ "is" ] )
    == Right [[RoleMember 1, RoleMember 2]]
  assertBool "two length-one paths" $ pathsToIts_sub_pRel
    ( Closed
      [ PNonRel $ It $ Just $ PExpr $ Phrase "a"
      , PNonRel $ It $ Just $ PExpr $ Phrase "b" ]
      [ "is" ] )
    == Right [[RoleMember 1],[RoleMember 2]]

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

  assertBool "HReach rightward" $
    ( pExprToHExpr (mkRslt mempty) <$>
      parse pReach "" "/tr a # /_" ) ==
    ( Right $ Right $
      HReach SearchRightward
      (HExpr $ ExprTplt [Phrase "",Phrase "",Phrase ""])
      (HExpr $ Phrase "a"))

  assertBool "HReach leftward" $
    ( pExprToHExpr (mkRslt mempty) <$>
      parse pReach "" "/tr /_ # a" ) ==
    ( Right $ Right $
      HReach SearchLeftward
      (HExpr $ ExprTplt [Phrase "",Phrase "",Phrase ""])
      (HExpr $ Phrase "a"))  

  assertBool "HTrans leftward, return leftward items" $
    ( pExprToHExpr (mkRslt mempty) <$>
      parse pPExpr "" "/trl (/it= a) # b" ) ==
    ( Right $ Right $ HTrans SearchLeftward [SearchLeftward]
      ( HExpr $ ExprTplt [Phrase "",Phrase "",Phrase ""])
      ( HExpr $ Phrase "b")
      ( HExpr $ Phrase "a") )

  assertBool "HTrans rightward, return leftward items" $
    ( pExprToHExpr (mkRslt mempty) <$>
      parse pPExpr "" "/trr (/it= a) # b" ) ==
    ( Right $ Right $ HTrans SearchRightward [SearchLeftward]
      ( HExpr $ ExprTplt [Phrase "",Phrase "",Phrase ""])
      ( HExpr $ Phrase "a")
      ( HExpr $ Phrase "b") )

  assertBool "HTrans rightward between disjunctions" $
    isRight ( fromRight (error "?") $
              pExprToHExpr (mkRslt mempty) <$>
              parse pPExpr "" "/trr (0 | 2) #< (1|4)" )

  assertBool "HTrans rightward between disjunctions with target" $
    isRight ( fromRight (error "?") $
              pExprToHExpr (mkRslt mempty) <$>
              parse pPExpr "" "/trr (/it=(0 | 2)) #< (1|4)" )


test_simplifyPExpr :: Test
test_simplifyPExpr = TestCase $ do
  assertBool "1" $
    simplifyPExpr ( PAnd [ PAnd [ PAnd [ PExpr $ Phrase "a"
                                       , PExpr $ Phrase "b" ]
                         , PAnd [ PAnd [ PExpr $ Phrase "c"
                                       , PExpr $ Phrase "d" ] ] ] ] )
    == PAnd (map (PExpr . Phrase) ["a","b","c","d"] )
