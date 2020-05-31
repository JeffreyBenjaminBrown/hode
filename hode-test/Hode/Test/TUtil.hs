{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TUtil where

import Test.HUnit
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char

import Hode.Rslt.Types
import Hode.Util.Alternation
import Hode.Util.Misc
import Hode.Util.Parse


test_module_util :: Test
test_module_util = TestList [
    TestLabel "test_tpltFromEithers" test_tpltFromEithers
  , TestLabel "test_transpose" test_transpose
  , TestLabel "test_beforeDuringAfter" test_beforeDuringAfter
  , TestLabel "test_nonPrefix" test_nonPrefix
  ]

test_nonPrefix :: Test
test_nonPrefix = TestCase $ do
  assertBool "" $ parse
    (many $ lexeme $ nonPrefix $ string "/_") ""
    "/_ /_ hoohaa" == Right ["/_","/_"]
  assertBool "it can't be a prefix" $
    isLeft $ parse
    (many $ lexeme $ nonPrefix $ string "/_") "" "/_f"
  assertBool "periods count as potentially the rest of the word" $
    isLeft $ parse
    (many $ lexeme $ nonPrefix $ string "/_") "" "/_."

test_beforeDuringAfter :: Test
test_beforeDuringAfter = TestCase $ do
  assertBool "" $ Right ([1,1],[9,9],[2,2]) ==
    beforeDuringAfter (> 5) [1,1,9,9,2,2]
  assertBool "" $ Right ([],[9,9],[2,2]) ==
    beforeDuringAfter (> 5) [9,9,2,2]
  assertBool "" $ isLeft $
    beforeDuringAfter (> 5) [1,1,9,9,2,2,9,9]

test_transpose :: Test
test_transpose = TestCase $ do
  assertBool "null" $ transpose [] == ([] :: [[Int]])
  assertBool "[[]]" $ transpose [[]] == ([] :: [[Int]])
  assertBool "trivial" $ transpose [[1]] == [[1]]
  assertBool "three cells" $ transpose [[1,2], [1]]
    == [[1,1], [2]]
  assertBool "big" $
    transpose [ [1,2,3]
              , [1    ]
              , [1,2  ] ]
    ==        [ [1,1,1]
              , [2,2]
              , [3] ]

test_tpltFromEithers :: Test
test_tpltFromEithers = TestCase $ do
  assertBool "full"              $ Tplt (Just 1) [2] (Just 3) ==
    ( tpltFromEithers [Right 1, Left (), Right 2, Left (), Right 3]
      :: Tplt Int )
  assertBool "missing left cap"  $ Tplt Nothing  [2] (Just 3) ==
    ( tpltFromEithers [Left (), Right 2, Left (), Right 3]
      :: Tplt Int )
  assertBool "missing right cap" $ Tplt (Just 1) [2] Nothing ==
    ( tpltFromEithers [Right 1, Left (), Right 2, Left ()]
      :: Tplt Int )
  assertBool "missing both caps" $ Tplt Nothing  [2] Nothing ==
    ( tpltFromEithers [Left (), Right 2, Left ()]
      :: Tplt Int )
