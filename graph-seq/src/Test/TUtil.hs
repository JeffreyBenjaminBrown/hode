{-# LANGUAGE ScopedTypeVariables #-}

module Test.TUtil where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Query
import Subst
import Types
import Util


testModuleUtil = TestList [
  TestLabel "testIsSubsetOfMap" testIsSubsetOfMap
  ]

testIsSubsetOfMap = TestCase $ do
  let m  = M.fromList [ (1, S.fromList [1,2]), (2, S.fromList [2,3]) ]
      m' = M.fromList [ (1, S.fromList [1,2]), (2, S.fromList [2,4]) ]
      n  = M.fromList [ (1, S.fromList [1,2]) ]
  assertBool "1" $       isSubsetOfMap n m
  assertBool "1" $ not $ isSubsetOfMap m n
  assertBool "1" $       isSubsetOfMap n m'
  assertBool "1" $ not $ isSubsetOfMap m m'
