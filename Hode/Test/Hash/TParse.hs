{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.TParse where

import           Data.Either (isLeft)
import qualified Data.Map as M
import           Text.Megaparsec
import           Test.HUnit

import Hode.Hash.Convert
import Hode.Hash.Parse
import Hode.Hash.Types
import Hode.Hash.Util
import Hode.Qseq.Types (Var(..))
import Hode.Rslt.Index
import Hode.Rslt.Types


vs :: String -> Var
vs = VarString

test_module_hash_parse :: Test
test_module_hash_parse = TestList [
    TestLabel "test_parse_rels" test_parse_rels
  , TestLabel "test_parse_pPExpr" test_parse_pPExpr
  , TestLabel "test_parse_hExpr" test_parse_hExpr
  , TestLabel "test_parse_tplt" test_parse_tplt
  , TestLabel "test_hashWord" test_hashWord
  , TestLabel "test_hashPhrase" test_hashPhrase
  , TestLabel "test_pAny" test_pAny
  , TestLabel "test_pIt" test_pIt
  ]

test_pIt :: Test
test_pIt = TestCase $ do
  assertBool "" $ parse pIt "" "/it" == Right (It Nothing)
  assertBool "PITFALL: This demonstrates an error: Hash expressions need to be prefixed with /h, unless they are 0-level words or phrases." $
    parse pIt ""                 "/it= 333 # 555" ==
    Right ( It $ Just $ PRel ( Open 1 [ PNonRel $ PExpr $ Phrase "333"
                                      , PNonRel $ PExpr $ Phrase "555" ]
                               [""] ) )
  assertBool "A 0-level phrase." $
    (simplifyPExpr <$> parse pIt "" "/it= 333 555") ==
    (Right $ It $ Just $ PExpr $ Phrase "333 555")

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
  assertBool "" $ parse hashPhrase "" "a b \"c d\" e f"
    == Right "a b c d e f"

test_hashWord :: Test
test_hashWord = TestCase $ do
  let s = "start!@%^*+=-`~_[]{}:;<>?,.finish"
      t = "start " ++ s ++ " finish"
  assertBool "" $ parse hashWord "" s == Right s
  assertBool "" $ parse hashWord "" t == Right "start"
  assertBool "" $ isLeft $ parse hashWord "" ""

test_parse_tplt :: Test
test_parse_tplt = TestCase $ do
  assertBool "1" $ parse _pTplt "parse error"
    "/_ sees /_ whenever there is /_"
    == Right ( ExprTplt $ Tplt
               Nothing
               [ Phrase "sees",
                 Phrase "whenever there is"]
               Nothing )

  assertBool "2" $ parse _pTplt "parse error"
    "remember /_ whenever there is /_"
    == Right ( ExprTplt $ Tplt
               ( Just $ Phrase "remember" )
               [ Phrase "whenever there is"]
               Nothing )

  assertBool "3" $ parse _pTplt "parse error"
    "/_ whenever there is /_ please"
    == Right ( ExprTplt $ Tplt
               Nothing
               [ Phrase "whenever there is"]
               ( Just $ Phrase "please" ) )

test_parse_hExpr :: Test
test_parse_hExpr = TestCase $ do
  let r = mkRslt mempty
  assertBool "1" $
    ( either (Left . show) Right
      ( parse pPExpr "doh!" "/hash /_ #e w" )
      >>= pExprToHExpr r)
    == ( Right $ HMap $ M.fromList
         [ ( RoleInRel' $ RoleTplt
           , HExpr $ ExprTplt $ Tplt Nothing [Phrase "e"] Nothing )
         , ( RoleInRel' $ RoleMember 2
           , HExpr $ Phrase "w" ) ] )

test_parse_pPExpr :: Test
test_parse_pPExpr = TestCase $ do
  assertBool "word" $ parse pPhrase "wut" "sammich bagel 1234"
    == Right (PExpr $ Phrase "sammich bagel 1234")

  assertBool "map" $ parse pMap "wut"
    ( "/map (1 /hash a) (2 /hash b) " ++
      "(tplt remember to /_ whenever there is /_ because /_)" )
    == Right
    ( PMap $ M.fromList
      [ ( RoleInRel' $ RoleMember 1, PExpr $ Phrase "a" )
      , ( RoleInRel' $ RoleMember 2, PExpr $ Phrase "b" )
      , ( RoleInRel' $ RoleTplt, PExpr $ ExprTplt $ Tplt
          ( Just $ Phrase "remember to" )
          [ Phrase "whenever there is",
            Phrase "because" ]
          Nothing ) ] )

  assertBool "any" $ parse pAny "any" "/_ "
    == Right Any
  assertBool "var" $ parse pVar "wut" "/var x1 "
    == Right (PVar $ vs "x1")
  assertBool "it nothing" $ parse pIt "wut" "/it "
    == Right (It Nothing)
  assertBool "it $ just a # b" $
    parse pIt "wut" "/it= /hash a # b" ==
    Right ( It $ Just $ PRel
            ( Open 1 [ PNonRel $ PExpr $ Phrase "a"
                     , PNonRel $ PExpr $ Phrase "b" ]
              [""] ) )

  assertBool "eval $ just a # b" $ parse pEval "wut" "/eval /hash a # b"
    == Right
    ( PEval $ PRel
      ( Open 1 [ PNonRel $ PExpr $ Phrase "a"
               , PNonRel $ PExpr $ Phrase "b" ]
        [""] ) )

  assertBool "/it= needs a space after" $
    parse pEval "" "/eval (/it= 0|1) # 2" == Right
    ( PEval $ PRel $ Open 1
      [ PNonRel $ It $ Just $ POr [ PExpr (Phrase "0"),
                                    PExpr (Phrase "1") ],
        PNonRel (PExpr $ Phrase "2")] [""])
  assertBool "/it= needs a space after" $
    isLeft $ parse pEval "" "/eval (/it=0|1) # 2"


test_parse_rels :: Test
test_parse_rels = TestCase $ do
  assertBool "1 level" $ parse pRel "wut" "a b #(w x) c d"
    == Right ( Open 1
               [ pnrPhrase "a b", pnrPhrase "c d"]
               [ "w x" ] )

  assertBool "1 level 2 separators" $ parse pRel "wut" "I #am ##because I #think"
    == Right ( Open 2
               [ Open 1
                 [ pnrPhrase "I", Absent]
                 [ "am" ]
               , Open 1
                 [pnrPhrase "I", Absent]
                 ["think"]
               ]
               [ "because" ] )

  assertBool "2 levels 2 separators" $ parse pRel "wut"
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
