{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TUtil where

import Test.HUnit

import Hode.Rslt.RTypes
import Hode.Util.Alternation


test_module_util :: Test
test_module_util = TestList [
  TestLabel "test_tpltFromEithers" test_tpltFromEithers
  ]

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
