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
import Hash.HTypes
import Hash.HParse
import Util.Parse


test_module_hash_parse = TestList [
    TestLabel "test_parse_rels" test_parse_rels
  ]

test_parse_rels = TestCase $ do
  assertBool "1" $ parse expr "wut" "a b #(w x) c d"
    == Right ( Open 1
               [ Leaf "a b", Leaf "c d"]
               [ "w x" ] )

  assertBool "2" $ parse expr "wut" "I #am ##because I #think"
    == Right ( Open 2
               [ Open 1
                 [ Leaf "I", Absent]
                 [ "am" ]
               , Open 1
                 [Leaf "I", Absent]
                 ["think"]
               ]
               [ "because" ] )

  assertBool "3" $
    parse expr "wut" "I #think ##therefore I #am thinking ##so #like yeah man"
    == Right ( Open 2 [ Open 1 [ Leaf "I"
                               , Absent] [ "think"]
                      , Open 1 [ Leaf "I"
                               , Leaf "thinking"] [ "am"]
                      , Open 1 [ Absent
                               , Leaf "yeah man"] [ "like"]]
               [ "therefore", "so"] )

  assertBool "4" $ parse expr "wut"
    "I #think ##therefore I #am thinking ###so #like yeah man"
    == Right ( Open 3
               [ Open 2
                 [ Open 1 [ Leaf "I", Absent ] [ "think" ]
                 , Open 1 [ Leaf "I", Leaf "thinking" ]
                   [ "am" ] ]
                 [ "therefore" ]
               , Open 1 [ Absent, Leaf "yeah man"]
                 [ "like"] ]
               [ "so" ] )
