{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.TParse where

import           Data.Either (isLeft)
import qualified Data.Map as M
import           Text.Megaparsec
import           Test.HUnit

import Hode.Hash.Convert
import Hode.Hash.HParse
import Hode.Hash.HTypes
import Hode.Hash.HUtil
import Hode.Qseq.QTypes (Var(..))
import Hode.Rslt.Index
import Hode.Rslt.RTypes


vs :: String -> Var
vs = VarString

test_module_hash_parse :: Test
test_module_hash_parse = TestList [
    TestLabel "test_parse_rels" test_parse_rels
  , TestLabel "test_parse_pPExpr" test_parse_pPExpr
  , TestLabel "test_parse_hExpr" test_parse_hExpr
  , TestLabel "test_parse_tplt" test_parse_tplt
  , TestLabel "test_hashIdentifier" test_hashIdentifier
  , TestLabel "test_hashPhrase" test_hashPhrase
  , TestLabel "test_pAny" test_pAny
  , TestLabel "test_pIt" test_pIt
  ]

test_pIt :: Test
test_pIt = TestCase $ do
  assertBool "" $ parse pIt "" "/it" == Right (It Nothing)
  assertBool "careful! hash expressions need to be prefixed with /h, unless they are 0-level phrases"
    ( parse pIt ""                 "/it= 333 # 555" ==
      Right (It $ Just $ PExpr $ Phrase "333") )
  assertBool "a 0-level phrase"
    ( parse pIt ""                 "/it= 333 555" ==
      Right (It $ Just $ PExpr $ Phrase "333 555") )
  assertBool "a hash expression" $
    parse pIt "" "/it= /h 3 # 4" ==
    Right ( It $ Just $ PRel ( Open 1 [ PNonRel $ PExpr $ Phrase "3"
                                      , PNonRel $ PExpr $ Phrase "4" ]
                               [""] ) )

test_pAny :: Test
test_pAny = TestCase $ do
  assertBool "1" $ parse pAny "" "/_"    == Right Any
  assertBool "2" $ parse pAny "" "/any " == Right Any

test_hashPhrase :: Test
test_hashPhrase = TestCase $ do
  let s = "one two three"
    in assertBool "" $ parse hashPhrase "" s == Right s
  assertBool "the empty phrase" $
    parse hashPhrase "" "\"\"" == Right ""
  assertBool "not the empty phrase" $
    isLeft $ parse hashPhrase "" ""

test_hashIdentifier :: Test
test_hashIdentifier = TestCase $ do
  let s = "start!@%^*+=-`~_[]{}:;<>?,.finish"
      t = "start " ++ s ++ " finish"
  assertBool "" $ parse hashIdentifier "" s == Right s
  assertBool "" $ parse hashIdentifier "" t == Right "start"
  assertBool "" $ isLeft $ parse hashIdentifier "" ""

test_parse_tplt :: Test
test_parse_tplt = TestCase $ do
  assertBool "non-present joints are represented as \\\"\\\""
    $ parse _pTplt "" "sees (whenever there is) \"\" "
    == Right ( ExprTplt [ Phrase "sees"
                        , Phrase "whenever there is"
                        , Phrase "" ] )

test_parse_hExpr :: Test
test_parse_hExpr = TestCase $ do
  let r = mkRslt mempty
  assertBool "1" $
    ( either (Left . show) Right
      ( parse pPExpr "doh!" "/hash /_ #e w" )
      >>= pExprToHExpr r)
    == ( Right $ HMap $ M.fromList
         [ ( RoleTplt
           , HExpr $ ExprTplt [ Phrase "",Phrase "e",Phrase "" ] )
         , ( RoleMember 2
           , HExpr $ Phrase "w" ) ] )

test_parse_pPExpr :: Test
test_parse_pPExpr = TestCase $ do
  assertBool "addr" $ parse pAddr "wut" "/addr 34 "
    == Right (Addr 34)
  assertBool "word" $ parse pPhrase "wut" "sammich bagel 1234"
    == Right (PExpr $ Phrase "sammich bagel 1234")

  assertBool "map" $ parse pMap "wut"
    "/map (1 /hash a) (2 /hash b) (tplt sees (whenever there is) because)"
    == Right
    ( PMap $ M.fromList
      [ ( RoleMember 1, PExpr $ Phrase "a" )
      , ( RoleMember 2, PExpr $ Phrase "b" )
      , ( RoleTplt, PExpr $ ExprTplt $ map Phrase
          ["sees","whenever there is","because"] )
      ] )

  assertBool "any" $ parse pAny "any" "/_ "
    == Right Any
  assertBool "var" $ parse pVar "wut" "/var x1 "
    == Right (PVar $ vs "x1")
  assertBool "it nothing" $ parse pIt "wut" "/it "
    == Right (It Nothing)
  assertBool "it $ just a # b" $ parse pIt "wut" "/it= /hash a # b" == Right
    ( It $ Just $ PRel
      ( Open 1 [ PNonRel $ PExpr $ Phrase "a"
               , PNonRel $ PExpr $ Phrase "b" ]
        [""] ) )

  assertBool "eval $ just a # b" $ parse pEval "wut" "/eval /hash a # b"
    == Right
    ( PEval $ PRel
      ( Open 1 [ PNonRel $ PExpr $ Phrase "a"
               , PNonRel $ PExpr $ Phrase "b" ]
        [""] ) )

test_parse_rels :: Test
test_parse_rels = TestCase $ do
  assertBool "1 level" $ parse pRel "wut" "a b #(w x) c d"
    == Right ( Open 1
               [ pnrPhrase "a b", pnrPhrase "c d"]
               [ "w x" ] )

  assertBool "1 level 2 joints" $ parse pRel "wut" "I #am ##because I #think"
    == Right ( Open 2
               [ Open 1
                 [ pnrPhrase "I", Absent]
                 [ "am" ]
               , Open 1
                 [pnrPhrase "I", Absent]
                 ["think"]
               ]
               [ "because" ] )

  assertBool "2 levels 2 joints" $ parse pRel "wut"
    "I #think ##therefore I #am thinking ##so #like yeah man"
    == Right ( Open 2 [ Open 1 [ pnrPhrase "I"
                               , Absent] [ "think"]
                      , Open 1 [ pnrPhrase "I"
                               , pnrPhrase "thinking"] [ "am"]
                      , Open 1 [ Absent
                               , pnrPhrase "yeah man"] [ "like"]]
               [ "therefore", "so"] )

  assertBool "3 levels" $ parse pRel "wut"
    "I #think ##therefore I #am thinking ###so #like yeah man"
    == Right ( Open 3
               [ Open 2
                 [ Open 1 [ pnrPhrase "I", Absent ] [ "think" ]
                 , Open 1 [ pnrPhrase "I", pnrPhrase "thinking" ]
                   [ "am" ] ]
                 [ "therefore" ]
               , Open 1 [ Absent, pnrPhrase "yeah man"]
                 [ "like"] ]
               [ "so" ] )

  assertBool "\\, 1 level" $ parse pRel "wut"
    "a \\ b"
    == Right ( PNonRel $ PDiff ( PExpr $ Phrase "a" )
                               ( PExpr $ Phrase "b" ) )

  assertBool "&, 1 level" $ parse pRel "wut"
    "a & b"
    == Right ( PNonRel $ PAnd [ PExpr $ Phrase "a"
                              , PExpr $ Phrase "b" ] )

  assertBool "& under #, 1 level" $ parse pRel "wut" "a & b ## c"
    == Right
    ( Open 2 [ PNonRel $ PAnd [ PExpr $ Phrase "a"
                              , PExpr $ Phrase "b" ]
             , PNonRel $ PExpr $ Phrase "c" ]
      [""] )

  assertBool "& over #, 1 level" $ parse pRel "wut" "a && b # c"
    == Right
    ( PNonRel $ PAnd [ PExpr $ Phrase "a"
                     , PRel $ Open 1 [ PNonRel $ PExpr $ Phrase "b"
                                     , PNonRel $ PExpr $ Phrase "c" ]
                       [""] ] )

  assertBool "2 levels of &, arities 2 and 3"
    $ parse pRel "wut" "a & b && c & d && e"
    == Right ( PNonRel $ PAnd
               $ map (PExpr . Phrase) ["a", "b", "c", "d", "e"] )

  assertBool "or, 1 levels" $ parse pRel "wut"
    "a | b"
    == Right ( PNonRel $ POr [ PExpr $ Phrase "a"
                             , PExpr $ Phrase "b" ] )

  assertBool "or, 2 levels" $ parse pRel "wut"
    "a | b || c | d"
    == Right ( PNonRel $ POr $ map (PExpr . Phrase) ["a","b","c","d"] )

  assertBool "3 levels of &, | and #"
    $ parse pRel "wut" "a # b && c | d ||| a ## b # c"
    == Right
    ( PNonRel
      ( POr
        [ PAnd
          [ PRel
            ( Open 1
              [ PNonRel ( PExpr ( Phrase "a"))
              , PNonRel ( PExpr ( Phrase "b"))]
              [ ""])
          , POr [ PExpr ( Phrase "c")
                , PExpr ( Phrase "d")]]
        , PRel ( Open 2 [ PNonRel ( PExpr ( Phrase "a"))
                        , Open 1 [ PNonRel ( PExpr ( Phrase "b"))
                                 , PNonRel ( PExpr ( Phrase "c"))]
                          [ ""]]
                 [ ""])]))

  assertBool "3 levels of \\, &, | and #, where the L in EInfixL matters"
    $ parse pRel "wut" "a # b \\\\ c | d ||| a && b \\\\ c"
    == Right
    ( PNonRel ( POr [ PDiff ( PRel ( Open 1
                                     [ PNonRel $ PExpr $ Phrase "a"
                                     , PNonRel $ PExpr $ Phrase "b" ]
                                     [""] ) )
                      ( POr [ PExpr $ Phrase "c"
                            , PExpr $ Phrase "d" ] )
                    , PDiff ( PAnd [ PExpr $ Phrase "a"
                                   , PExpr $ Phrase "b" ] )
                      ( PExpr $ Phrase "c" ) ] ) )
