{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TBrick where

import           Test.HUnit

import Hode.Brick


test_module_hode_brick :: Test
test_module_hode_brick = TestList [
  TestLabel "test_attrStrip" test_attrStrip
  ]

test_attrStrip :: Test
test_attrStrip = TestCase $ do
  assertBool "0" $ attrStrip [] == ([] :: AttrString)
  assertBool "1" $ attrStrip [(" x ",())] == [("x",())]
  assertBool "2" $ attrStrip [ (" x ",())
                             , (" x ",()) ] == [ ("x ",())
                                               , (" x",())
                                               ]
  assertBool "3" $ attrStrip [ (" x ",())
                             , (" x ",())
                             , (" x ",()) ] == [ ("x ",())
                                               , (" x ",())
                                               , (" x",()) ]

x = attrStrip [ (" x ",())
              , (" x ",())
              , (" x ",()) ]
