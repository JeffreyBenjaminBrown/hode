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
import Util.UParse


test_module_hash_parse = TestList [
    TestLabel "test_parse_rels" test_parse_rels
  ]

test_parse_rels = TestCase $ do
  assertBool "1" $ parse expr "wut" "a b #(w x) c d"
    == Right ( Open 1
               [ pWord "a b", pWord "c d"]
               [ "w x" ] )

  assertBool "2" $ parse expr "wut" "I #am ##because I #think"
    == Right ( Open 2
               [ Open 1
                 [ pWord "I", Absent]
                 [ "am" ]
               , Open 1
                 [pWord "I", Absent]
                 ["think"]
               ]
               [ "because" ] )

  assertBool "3" $
    parse expr "wut" "I #think ##therefore I #am thinking ##so #like yeah man"
    == Right ( Open 2 [ Open 1 [ pWord "I"
                               , Absent] [ "think"]
                      , Open 1 [ pWord "I"
                               , pWord "thinking"] [ "am"]
                      , Open 1 [ Absent
                               , pWord "yeah man"] [ "like"]]
               [ "therefore", "so"] )

  assertBool "4" $ parse expr "wut"
    "I #think ##therefore I #am thinking ###so #like yeah man"
    == Right ( Open 3
               [ Open 2
                 [ Open 1 [ pWord "I", Absent ] [ "think" ]
                 , Open 1 [ pWord "I", pWord "thinking" ]
                   [ "am" ] ]
                 [ "therefore" ]
               , Open 1 [ Absent, pWord "yeah man"]
                 [ "like"] ]
               [ "so" ] )
