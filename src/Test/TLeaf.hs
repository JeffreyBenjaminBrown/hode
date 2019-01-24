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
import Types


test_modules_leaf = TestList [
    TestLabel "test_runVarTest" test_runVarTest
  ]

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
