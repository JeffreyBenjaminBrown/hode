{-# LANGUAGE ScopedTypeVariables #-}

module Test.TQuery where

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


testModuleQuery = TestList [
  TestLabel "testFindable" testFindable
  , TestLabel "testDisjointExistentials" testDisjointExistentials
  ]

testDisjointExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = Var "x" $ S.empty
      y   = Var "y" $ S.empty
      qx  = ForSome x qf
      qy  = ForSome y qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ disjointExistentials (ForSome x qx)  == False
  assertBool "2" $ disjointExistentials (ForSome y qx)  == True
  assertBool "3" $ disjointExistentials qxy             == True
  assertBool "4" $ disjointExistentials (QAnd [qx,qxy]) == False

testFindable = TestCase $ do
  let qf = QFind $ Find (\_ _    -> S.empty) S.empty
      qc = QCond $ Cond (\_ _  _ -> False  ) S.empty
  assertBool "1" $ findable (QAnd [qf, qc]) == True
  assertBool "2" $ findable (QAnd [qf]    ) == True
  assertBool "3" $ findable (QAnd [qc]    ) == False
  assertBool "4" $ findable (QAnd []      ) == False
  assertBool "5" $ findable (QOr  [qf, qc]) == False
  assertBool "6" $ findable (QOr  [qf]    ) == True
  assertBool "7" $ findable (QOr  [qc]    ) == False
  assertBool "8" $ findable (QOr  []      ) == False

