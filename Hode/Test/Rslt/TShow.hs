{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TShow where

import           Test.HUnit

import           Hode.Rslt.RTypes
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_show :: Test
test_module_rslt_show = TestList [
  TestLabel "test_hashUnlessEmptyStartOrEnd" test_hashUnlessEmptyStartOrEnd
  , TestLabel "test_eShow" test_eShow
  ]

test_eShow :: Test
test_eShow = TestCase $ do
  assertBool "1" $ eShow D.rslt (Phrase "hello") == Right "hello"
  assertBool "2" $ eShow D.rslt (ExprTplt $ map Phrase ["a","b","c"] )
    == Right "a _ b _ c"
  assertBool "3" $ eShow D.rslt
    ( ExprRel ( Rel ( map Phrase ["a","b"] )
                $ ExprTplt $ map Phrase ["","=",""] ) )
    == Right "a #= b"

test_hashUnlessEmptyStartOrEnd :: Test
test_hashUnlessEmptyStartOrEnd = TestCase $ do
  assertBool "1" $ hashUnlessEmptyStartOrEnd 2 [] == []
  assertBool "2" $ hashUnlessEmptyStartOrEnd 2 [""] == [""]
  assertBool "3" $ hashUnlessEmptyStartOrEnd 2 ["",""] == ["",""]
  assertBool "4" $ hashUnlessEmptyStartOrEnd 2 ["","",""] == ["","##",""]
  assertBool "5" $ hashUnlessEmptyStartOrEnd 2 ["a","b",""]
    == ["##a","##b",""]
  assertBool "6" $ hashUnlessEmptyStartOrEnd 2 ["","b","c"]
    == ["","##b","##c"]
