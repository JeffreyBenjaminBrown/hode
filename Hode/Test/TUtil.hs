{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TUtil where

import Test.HUnit
import Data.Either

import Hode.Util.Misc
import Hode.Rslt.Types
import Hode.Util.Alternation


test_module_util :: Test
test_module_util = TestList [
    TestLabel "test_tpltFromEithers" test_tpltFromEithers
  , TestLabel "test_transpose" test_transpose
  , TestLabel "test_uniqueContiguousSublist" test_uniqueContiguousSublist
  ]

test_uniqueContiguousSublist :: Test
test_uniqueContiguousSublist = TestCase $ do
  assertBool "" $ Right [9,9] ==
    uniqueContiguousSublist (> 5) [1,1,9,9,1,1]
  assertBool "" $ Right [9,9] ==
    uniqueContiguousSublist (> 5) [9,9,1,1]
  assertBool "" $ isLeft $
    uniqueContiguousSublist (> 5) [1,1,9,9,1,1,9,9]

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
