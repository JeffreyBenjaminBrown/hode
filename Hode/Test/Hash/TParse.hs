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
  , TestLabel "test_pPhrase" test_pPhrase
  , TestLabel "test_pMember" test_pMember
  , TestLabel "test_pEval" test_pEval
  , TestLabel "test_pMap" test_pMap
  , TestLabel "test_parse_hExpr" test_parse_hExpr
  , TestLabel "test_parse_tplt" test_parse_tplt
  , TestLabel "test_hashWord" test_hashWord
  , TestLabel "test_hashPhrase" test_hashPhrase
  , TestLabel "test_pAllTplts" test_pAllTplts
  , TestLabel "test_pAny" test_pAny
  , TestLabel "test_pVar" test_pVar
  , TestLabel "test_pIt" test_pIt
  , TestLabel "test_pInvolves" test_pInvolves
  , TestLabel "test_pAddr" test_pAddr
  , TestLabel "test_pHashExpr" test_pHashExpr
  ]

test_pHashExpr :: Test
test_pHashExpr = TestCase $ do
  assertBool "it $ just a # b" $
    parse pIt "a" "/it= /hash a # b" ==
    parse pIt "b" "/it=       a # b"

  assertBool "eval $ just a # b" $
    parse pEval "a" "/eval /hash a # b" ==
    parse pEval "b" "/eval       a # b"

  assertBool "eval $ just a # b" $
    parse pRel "a" "/hash (a # /hash b)" ==
    parse pRel "b"       "(a #       b)"

  assertBool "map" $
    parse pMap "a"
    ( "/map (1 /hash a) (2 /hash b) " ++
      "(tplt remember to /_ whenever there is /_ because /_)" )
    == parse pMap "a"
    ( "/map (1       a) (2       b) " ++
      "(tplt remember to /_ whenever there is /_ because /_)" )

test_pAddr :: Test
test_pAddr = TestCase $ do
  let a = parse pAddrs ""    "/@ 0 2-4 6"
      b = parse pAddrs "" "/addr 0 2-4 6"
  assertBool "" $ a == b
  assertBool "" $ a == Right
    (POr $ map (PExpr . ExprAddr) [0,2,3,4,6] )

test_pInvolves :: Test
test_pInvolves = TestCase $ do
  assertBool "" $
    parse pInvolves "" "/i-2 a" == Right
    (PInvolves 2 $ PRel $ PNonRel $ PExpr $ Phrase "a")

test_pIt :: Test
test_pIt = TestCase $ do
  assertBool "it is unspecified" $
    parse pIt "" "/it" == Right (It Nothing)
  assertBool "It= a pRel." $
    parse pIt ""                 "/it= 333 # 555" ==
    Right ( It $ Just $ PRel ( Open 1 [ PNonRel $ PExpr $ Phrase "333"
                                      , PNonRel $ PExpr $ Phrase "555" ]
                               [""] ) )
  assertBool "It= a 0-level phrase." $
    (simplifyPExpr <$> parse pIt "" "/it= 333 555") ==
    (Right $ It $ Just $ PExpr $ Phrase "333 555")

test_pMember :: Test
test_pMember = TestCase $ do
  assertBool "one /member" $
    parse pMember "" "/member a"
    == Right (PMember $ PRel $ PNonRel $ PExpr $ Phrase "a")
  assertBool "one /m" $
    parse pMember "" "/m a"
    == Right (PMember $ PRel $ PNonRel $ PExpr $ Phrase "a")

  assertBool "/m a # b" $
    parse pMember "" "/m a # b"
    == Right ( PMember $ PRel $
               Open 1 [ PNonRel $ PExpr $ Phrase "a",
                        PNonRel $ PExpr $ Phrase "b"]
               [""] )

  assertBool "not a prefix" $ isLeft $
    parse pMember "" "/m@m"

test_pAllTplts :: Test
test_pAllTplts = TestCase $ do
  assertBool "one /ts"
    $ parse pAllTplts "" "/ts"         == Right PTplts
  assertBool "one /tplts"
    $ parse pAllTplts "" "/tplts "     == Right PTplts
  assertBool "one /templates"
    $ parse pAllTplts "" "/templates " == Right PTplts

  assertBool "not a prefix" $ isLeft $
    parse pAllTplts "" "/templates-are-cool"
  assertBool "many `/ts`s" $ parse (many pAllTplts) "" "/ts /ts /ts /ts"
    == Right (take 4 $ repeat PTplts)

test_pAny :: Test
test_pAny = TestCase $ do
  assertBool "not a prefix" $ isLeft $
    parse pAny "" "/_."
  assertBool "one /_" $ Right Any ==
    parse pAny "" "/_"
  assertBool "one /any" $ parse pAny "" "/any " == Right Any
  assertBool "many /_s" $ parse (many pAny) "" "/_ /_ /_ /_"
    == Right (take 4 $ repeat Any)

test_pVar :: Test
test_pVar = TestCase $ do
  assertBool "not a prefix" $ isLeft $
    parse pAny "" "/v."
  assertBool "some `/v`s" $ parse (many pVar) "" "/v a /v b"
    == Right (map (PVar . VarString) ["a","b"])
  assertBool "some `/var`s" $ parse (many pVar) "" "/var a /var   b    "
    == Right (map (PVar . VarString) ["a","b"])

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
  assertBool "_pTplt is like pTpl" $ let
    a = parse _pTplt "parse error"    "/_ x /_ y"
    b = parse  pTplt "parse error" "/t /_ x /_ y"
    in a == Right ( ExprTplt $ Tplt
                    Nothing
                    [Phrase "x"]
                    (Just $ Phrase "y") )
       && a == b

  assertBool "no caps" $ parse _pTplt "parse error"
    "/_ sees /_ when /_"
    == Right ( ExprTplt $ Tplt
               Nothing
               [ Phrase "sees",
                 Phrase "when"]
               Nothing )

  assertBool "left cap" $ parse _pTplt "parse error"
    "remember /_ when /_"
    == Right ( ExprTplt $ Tplt
               ( Just $ Phrase "remember" )
               [ Phrase "when"]
               Nothing )

  assertBool "right cap" $ parse _pTplt "parse error"
    "/_ when /_ please"
    == Right ( ExprTplt $ Tplt
               Nothing
               [ Phrase "when"]
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

test_pPhrase :: Test
test_pPhrase = TestCase $ do
  assertBool "phrase" $ parse pPhrase "" "sammich bagel 1234"
    == Right (PExpr $ Phrase "sammich bagel 1234")
  assertBool "phrase with weird chars" $
    parse pPhrase "" "sammich bagel 1234!@#$% ..."
    == Right (PExpr $ Phrase "sammich bagel 1234!@#$% ...")

test_pMap :: Test
test_pMap = TestCase $ do
  assertBool "map" $ parse pMap "wut"
    ( "/map (1 a) (2 b) " ++
      "(tplt remember to /_ when there is /_ because /_)" )
    == Right
    ( PMap $ M.fromList
      [ ( RoleInRel' $ RoleMember 1, PExpr $ Phrase "a" )
      , ( RoleInRel' $ RoleMember 2, PExpr $ Phrase "b" )
      , ( RoleInRel' $ RoleTplt, PExpr $ ExprTplt $ Tplt
          ( Just $ Phrase "remember to" )
          [ Phrase "when there is",
            Phrase "because" ]
          Nothing ) ] )

test_pEval :: Test
test_pEval = TestCase $ do
  assertBool "eval $ just a # b" $ parse pEval "wut" "/eval a # b"
    == Right
    ( PEval $ PRel
      ( Open 1 [ PNonRel $ PExpr $ Phrase "a"
               , PNonRel $ PExpr $ Phrase "b" ]
        [""] ) )

  assertBool "/eval (/it= 0 /| 1) # 2" $
    parse pEval "" "/eval (/it= 0 /| 1) # 2" == Right
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
    "a /\\ b"
    == Right ( PNonRel $ PDiff ( PExpr $ Phrase "a" )
                               ( PExpr $ Phrase "b" ) )

  assertBool "&, 1 level" $ parse pRel "wut"
    "a /& b"
    == Right ( PNonRel $ PAnd [ PExpr $ Phrase "a"
                              , PExpr $ Phrase "b" ] )

  assertBool "& under #, 1 level" $ parse pRel "wut" "a /& b ## c"
    == Right
    ( Open 2 [ PNonRel $ PAnd [ PExpr $ Phrase "a"
                              , PExpr $ Phrase "b" ]
             , PNonRel $ PExpr $ Phrase "c" ]
      [""] )

  assertBool "& over #, 1 level" $ parse pRel "wut" "a /&& b # c"
    == Right
    ( PNonRel $ PAnd [ PExpr $ Phrase "a"
                     , PRel $ Open 1 [ PNonRel $ PExpr $ Phrase "b"
                                     , PNonRel $ PExpr $ Phrase "c" ]
                       [""] ] )

  assertBool "2 levels of &, arities 2 and 3"
    $ parse pRel "wut" "a /& b /&& c /& d /&& e"
    == Right ( PNonRel $ PAnd
               $ map (PExpr . Phrase) ["a", "b", "c", "d", "e"] )

  assertBool "or, 1 levels" $ parse pRel "wut"
    "a /| b"
    == Right ( PNonRel $ POr [ PExpr $ Phrase "a"
                             , PExpr $ Phrase "b" ] )

  assertBool "or, 2 levels" $ parse pRel "wut"
    "a /| b /|| c /| d"
    == Right ( PNonRel $ POr $ map (PExpr . Phrase) ["a","b","c","d"] )

  assertBool "3 levels of &, | and #"
    $ parse pRel "wut" "a # b /&& c /| d /||| a ## b # c"
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
    $ parse pRel "wut" "a # b /\\\\ c /| d /||| a /&& b /\\\\ c"
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
