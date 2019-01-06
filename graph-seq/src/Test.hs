module Test where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Query
import Types


tests = runTestTT $ TestList
  [ TestLabel "testInvertMapToSet" testInvertMapToSet
  , TestLabel "testFindable" testFindable
  , TestLabel "testValidExistentials" testValidExistentials
  ]

testValidExistentials = TestCase $ do
  let qf  = QFind $ \_ _ -> S.empty
      x   = S.singleton $ VarFunc (Var "x") $ S.empty
      y   = S.singleton $ VarFunc (Var "y") $ S.empty
      qx  = ForSome x qf
      qy  = ForSome y qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ validExistentials (ForSome x qx)  == False
  assertBool "2" $ validExistentials (ForSome y qx)  == True
  assertBool "3" $ validExistentials qxy             == True
  assertBool "3" $ validExistentials (QAnd [qx,qxy]) == False

testFindable = TestCase $ do
  let qf = QFind $ \_ _    -> S.empty
      qc = QCond $ \_ _  _ -> False
  assertBool "1" $ findable (QAnd [qf, qc]) == True
  assertBool "2" $ findable (QAnd [qf]    ) == True
  assertBool "3" $ findable (QAnd [qc]    ) == False
  assertBool "4" $ findable (QAnd []      ) == False
  assertBool "5" $ findable (QOr  [qf, qc]) == False
  assertBool "6" $ findable (QOr  [qf]    ) == True
  assertBool "7" $ findable (QOr  [qc]    ) == False
  assertBool "8" $ findable (QOr  []      ) == False

testInvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet g == h
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet h == g
