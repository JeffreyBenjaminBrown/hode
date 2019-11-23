{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TBrick where

import           Test.HUnit

import Hode.Brick
import Hode.Brick.Wrap


test_module_hode_brick :: Test
test_module_hode_brick = TestList [
    TestLabel "test_colorStrip" test_colorStrip
  , TestLabel "test_colorStringLength" test_colorStringLength
  , TestLabel "test_splitAtLastSpaceBefore"
    test_splitAtLastSpaceBefore
  , TestLabel "test_extractLine" test_extractLine
  , TestLabel "test_toLines" test_toLines
  ]

test_toLines :: Test
test_toLines = TestCase $ do
  assertBool "" $ toLines 0 [] == [[]]
  assertBool "" $ toLines 4 [] == [[]]

test_extractLine :: Test
test_extractLine = TestCase $ do
  assertBool "" $ ([],[]) ==
    extractLine (error "meh") ([] :: ColorString)
  assertBool "stops once its length is greater than 2" $
    extractLine 2 [("a",1),("b",2),("c",3),("dd",4 :: Int)]
    == ([("a",1),("b",2)],
        [("c",3),("dd",4)])
  assertBool "never becomes greater than 5" $
    let s = [("a",1),("b",2),("c",3::Int)]
    in extractLine 5 s == (s,[])

test_splitAtLastSpaceBefore :: Test
test_splitAtLastSpaceBefore = TestCase $ do
  assertBool "" $
    splitAtLastSpaceBefore 6 ("",())
    == (("",()),("",()))
  assertBool "" $
    splitAtLastSpaceBefore 6 ("ab cd ef gh",())
    == (("ab cd ",()), ("ef gh",()))
  assertBool "" $
    splitAtLastSpaceBefore 6 ("abc def ghi jkl",())
    == (("abc ",()), ("def ghi jkl",()))
  assertBool "" $
    splitAtLastSpaceBefore 4 ("abcdabcd",())
    == (("abcd",()), ("abcd",()))


test_colorStringLength :: Test
test_colorStringLength = TestCase $ do
  let irr = error "irrelevant"
  assertBool "" $ colorStringLength [] == 0
  assertBool "" $ colorStringLength [("hi!", irr)] == 3
  assertBool "" $ colorStringLength [("hi!", irr),
                                    ("bye!",irr)] == 7

test_colorStrip :: Test
test_colorStrip = TestCase $ do
  assertBool "0" $ colorStrip [] == ([] :: ColorString)
  assertBool "1" $ colorStrip [(" x ",())] == [("x",())]
  assertBool "2" $ colorStrip [ (" x ",())
                             , (" x ",()) ] == [ ("x ",())
                                               , (" x",())
                                               ]
  assertBool "3" $ colorStrip [ (" x ",())
                             , (" x ",())
                             , (" x ",()) ] == [ ("x ",())
                                               , (" x ",())
                                               , (" x",()) ]
