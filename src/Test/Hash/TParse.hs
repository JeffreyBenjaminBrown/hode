{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hash.TParse where

import           Control.Monad (void)
import           Data.List (intersperse)
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Test.HUnit

import Hash.EitherExpr
import Hash.HParse
import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes
import Util.UParse


test_module_hash_parse = TestList [
    TestLabel "test_parse_rels" test_parse_rels
  , TestLabel "test_parse_pExpr" test_parse_pExpr
  ]

test_parse_pExpr = TestCase $ do
  assertBool "addr" $ parse pAddr "wut" "/addr 34 "
    == Right (PExpr $ ExprAddr 34)
  assertBool "word" $ parse pWord "wut" "sammich bagel 1234"
    == Right (PExpr $ Word "sammich bagel 1234")
  assertBool "any" $ parse pAny "any" "_ "
    == Right Any
  assertBool "var" $ parse pVar "wut" "/var x1 "
    == Right (PVar "x1")
  assertBool "it nothing" $ parse pIt "wut" "/it "
    == Right (It Nothing)
  assertBool "it $ just _" $ False


test_parse_rels = TestCase $ do
  assertBool "1" $ parse pRel "wut" "a b #(w x) c d"
    == Right ( Open 1
               [ pnrWord "a b", pnrWord "c d"]
               [ "w x" ] )

  assertBool "2" $ parse pRel "wut" "I #am ##because I #think"
    == Right ( Open 2
               [ Open 1
                 [ pnrWord "I", Absent]
                 [ "am" ]
               , Open 1
                 [pnrWord "I", Absent]
                 ["think"]
               ]
               [ "because" ] )

  assertBool "3" $ parse pRel "wut"
    "I #think ##therefore I #am thinking ##so #like yeah man"
    == Right ( Open 2 [ Open 1 [ pnrWord "I"
                               , Absent] [ "think"]
                      , Open 1 [ pnrWord "I"
                               , pnrWord "thinking"] [ "am"]
                      , Open 1 [ Absent
                               , pnrWord "yeah man"] [ "like"]]
               [ "therefore", "so"] )

  assertBool "4" $ parse pRel "wut"
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
