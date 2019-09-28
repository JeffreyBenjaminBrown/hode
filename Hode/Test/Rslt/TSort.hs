{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TSort where

import qualified Data.Set       as S
import           Test.HUnit

import Hode.Rslt.Index
import Hode.Rslt.Sort
import Hode.UI.NoUI


test_module_rslt_sort :: Test
test_module_rslt_sort = TestList [
  TestLabel "test_nothingIsGreater" test_nothingIsGreater
  ]

test_nothingIsGreater :: Test
test_nothingIsGreater = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 # 1", "1 # 2" ]
      Right t = head . S.toList <$> nFindAddrs r "/t /_ \"\" /_"
      Right number_0 = head . S.toList <$> nFindAddrs r "0"
      Right number_1 = head . S.toList <$> nFindAddrs r "1"
      Right number_2 = head . S.toList <$> nFindAddrs r "2"

  assertBool "If left is bigger, 0 is maximal." $ Right True ==
    maximal r (LeftIsBigger, t) number_0
  assertBool "If right is bigger, it's not." $ Right False ==
    maximal r (RightIsBigger, t) number_0

  assertBool "If left is bigger, 2 is not maximal." $ Right False ==
    maximal r (LeftIsBigger, t) number_2
  assertBool "If right is bigger, then it is." $ Right True ==
    maximal r (RightIsBigger, t) number_2

  assertBool "1 is not maximal under either orientation." $ Right False ==
    maximal r (LeftIsBigger, t) number_1
  assertBool "Ditto." $ Right False ==
    maximal r (RightIsBigger, t) number_1
