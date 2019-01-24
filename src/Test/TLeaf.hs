{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Test.TLeaf where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Query.Inspect
import Query.MkLeaf
import Query.RunLeaf
import Space.Graph
import Space.Graph.GQuery
import Types


test_modules_leaf = TestList [
    TestLabel "test_runVarTest" test_runVarTest
  , TestLabel "test_runFind" test_runFind
  ]

test_runFind = TestCase $ do
  let (a,b,c) = ("a","b","c")
      g = graph [ (1, [2,3] ) ]
      s = M.fromList [(a, 1), (b, 2)]
  assertBool "1" $ runFind g s (findChildren $ Left 1)  ==
    M.fromList [ (2, S.singleton M.empty)
               , (3, S.singleton M.empty) ]
  assertBool "2" $ runFind g s (findChildren $ Right a) ==
    M.fromList [ (2, S.singleton $ M.singleton a 1)
               , (3, S.singleton $ M.singleton a 1) ]
  assertBool "2" $ runFind g s (findChildren $ Right b) == M.empty

test_runVarTest = TestCase $ do
  --runVarTest :: sp -> Subst e -> VarTest e sp -> Bool
  let a_lt_1 = mkVarTest (>) (Left 1)    $ Right "a"
      b_lt_1 = mkVarTest (>) (Left 1)    $ Right "b"
      a_gt_b = mkVarTest (>) (Right "a") $ Right "b"
      b_gt_a = mkVarTest (>) (Right "b") $ Right "a"
      subst = M.fromList [("a",0),("b",2)] :: Subst Int
  assertBool "1" $ True  == runVarTest (error "whatever") subst a_lt_1
  assertBool "2" $ False == runVarTest (error "whatever") subst b_lt_1
  assertBool "3" $ False == runVarTest (error "whatever") subst a_gt_b
  assertBool "4" $ True == runVarTest (error "whatever") subst b_gt_a
