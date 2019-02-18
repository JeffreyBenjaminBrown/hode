{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hash.TParse where

import qualified Data.Map as M
import           Text.Megaparsec
import           Test.HUnit

import Hash.HParse
import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes


test_module_hash_parse :: Test
test_module_hash_parse = TestList [
    TestLabel "test_parse_rels" test_parse_rels
  , TestLabel "test_parse_pExpr" test_parse_pExpr
  ]

test_parse_pExpr :: Test
test_parse_pExpr = TestCase $ do
  assertBool "addr" $ parse pAddr "wut" "/addr 34 "
    == Right (Addr 34)
  assertBool "word" $ parse pWord "wut" "sammich bagel 1234"
    == Right (PExpr $ Word "sammich bagel 1234")

  assertBool "map" $ parse pMap "wut"
    "/map (1 /hash a) (2 /hash b) (tplt sees (whenever there is) because)"
    == Right
    ( PMap $ M.fromList
      [ ( RoleMember 1, PExpr $ Word "a" )
      , ( RoleMember 2, PExpr $ Word "b" )
      , ( RoleTplt, PExpr $ Tplt $ map Word
          ["sees","whenever there is","because"] )
      ] )

  assertBool "any" $ parse pAny "any" "_ "
    == Right Any
  assertBool "var" $ parse pVar "wut" "/var x1 "
    == Right (PVar "x1")
  assertBool "it nothing" $ parse pIt "wut" "/it "
    == Right (It Nothing)
  assertBool "it $ just a # b" $ parse pIt "wut" "/it= /hash a # b" == Right
    ( It $ Just $ PRel
      ( Open 1 [ PNonRel $ PExpr $ Word "a"
               , PNonRel $ PExpr $ Word "b" ]
        [""] ) )

  assertBool "eval $ just a # b" $ parse pEval "wut" "/eval /hash a # b"
    == Right
    ( PEval $ PRel
      ( Open 1 [ PNonRel $ PExpr $ Word "a"
               , PNonRel $ PExpr $ Word "b" ]
        [""] ) )

  assertBool "par, simplest" $ parse pPar "wut" "/par a" == Right
    (PPar [] "a")

  assertBool "par, balanced, one sub-Expr" $
    parse pPar "wut" "/par a (/hash x # y) b"
    == Right
    ( PPar [ ( "a"
             , PRel $ ( Open 1 [ PNonRel $ PExpr $ Word "x"
                               , PNonRel $ PExpr $ Word "y" ]
                        [""] ) ) ]
      "b" )

  assertBool "par, balanced, two sub-Exprs" $
    ( simplifyPExpr <$> parse pPar "wut"
      "/par a a (/hash x x #(j j) y y) b b (/hash z z) c d" )
    == Right
    ( PPar [ ( "a a"
             , PRel $ ( Open 1 [ PNonRel $ PExpr $ Word "x x"
                               , PNonRel $ PExpr $ Word "y y" ]
                        ["j j"] ) )
           , ( "b b", PExpr $ Word "z z" )
           ]
      "c d" )

  assertBool "par, left-absent, two sub-Exprs" $
    ( simplifyPExpr <$> parse pPar "wut"
      "/par (/hash x x #(j j) y y) b b (/hash z z) c d" )
    == Right
    ( PPar [ ( ""
             , PRel $ ( Open 1 [ PNonRel $ PExpr $ Word "x x"
                               , PNonRel $ PExpr $ Word "y y" ]
                        ["j j"] ) )
           , ( "b b", PExpr $ Word "z z" )
           ]
      "c d" )

  assertBool "par, right-absent, two sub-Exprs" $
    ( simplifyPExpr <$> parse pPar "wut"
      "/par (/hash x x #(j j) y y) b b (/hash z z)" )
    == Right
    ( PPar [ ( ""
             , PRel $ ( Open 1 [ PNonRel $ PExpr $ Word "x x"
                               , PNonRel $ PExpr $ Word "y y" ]
                        ["j j"] ) )
           , ( "b b", PExpr $ Word "z z" )
           ]
      "" )

  assertBool "par, middle-absent, two sub-Exprs" $
    ( simplifyPExpr <$> parse pPar "wut"
      "/par (/hash x x #(j j) y y) (/hash z z)" )
    == Right
    ( PPar [ ( ""
             , PRel $ ( Open 1 [ PNonRel $ PExpr $ Word "x x"
                               , PNonRel $ PExpr $ Word "y y" ]
                        ["j j"] ) )
           , ( "", PExpr $ Word "z z" )
           ]
      "" )

test_parse_rels :: Test
test_parse_rels = TestCase $ do
  assertBool "1 level" $ parse pRel "wut" "a b #(w x) c d"
    == Right ( Open 1
               [ pnrWord "a b", pnrWord "c d"]
               [ "w x" ] )

  assertBool "1 level 2 joints" $ parse pRel "wut" "I #am ##because I #think"
    == Right ( Open 2
               [ Open 1
                 [ pnrWord "I", Absent]
                 [ "am" ]
               , Open 1
                 [pnrWord "I", Absent]
                 ["think"]
               ]
               [ "because" ] )

  assertBool "2 levels 2 joints" $ parse pRel "wut"
    "I #think ##therefore I #am thinking ##so #like yeah man"
    == Right ( Open 2 [ Open 1 [ pnrWord "I"
                               , Absent] [ "think"]
                      , Open 1 [ pnrWord "I"
                               , pnrWord "thinking"] [ "am"]
                      , Open 1 [ Absent
                               , pnrWord "yeah man"] [ "like"]]
               [ "therefore", "so"] )

  assertBool "3 levels" $ parse pRel "wut"
    "I #think ##therefore I #am thinking ###so #like yeah man"
    == Right ( Open 3
               [ Open 2
                 [ Open 1 [ pnrWord "I", Absent ] [ "think" ]
                 , Open 1 [ pnrWord "I", pnrWord "thinking" ]
                   [ "am" ] ]
                 [ "therefore" ]
               , Open 1 [ Absent, pnrWord "yeah man"]
                 [ "like"] ]
               [ "so" ] )

  assertBool "and, 0 levels" $ parse pRel "wut"
    "a & b"
    == Right ( PNonRel $ PAnd [ PExpr $ Word "a"
                              , PExpr $ Word "b" ] )

  assertBool "& under #, 1 level" $ parse pRel "wut" "a & b ## c"
    == Right
    ( Open 2 [ PNonRel $ PAnd [ PExpr $ Word "a"
                              , PExpr $ Word "b" ]
             , PNonRel $ PExpr $ Word "c" ]
      [""] )

  assertBool "& over #, 1 level" $ parse pRel "wut" "a && b # c"
    == Right
    ( PNonRel $ PAnd [ PExpr $ Word "a"
                     , PRel $ Open 1 [ PNonRel $ PExpr $ Word "b"
                                     , PNonRel $ PExpr $ Word "c" ]
                       [""] ] )

  assertBool "2 levels of &, arities 2 and 3"
    $ parse pRel "wut" "a & b && c & d && e"
    == Right ( PNonRel $ PAnd
               $ map (PExpr . Word) ["a", "b", "c", "d", "e"] )
