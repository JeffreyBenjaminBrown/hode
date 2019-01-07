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
  , TestLabel "testReconcile2" testReconcile2
  , TestLabel "testReconcile1toMany" testReconcile1toMany
  , TestLabel "testReconcile" testReconcile
  ]

testReconcile = TestCase $ do
  let x1    = S.singleton ( M.singleton (Var "x") 1 )
      x1_x2 = S.fromList  [ M.singleton (Var "x") 1
                          , M.singleton (Var "x") 2 ]
      x1_x3 = S.fromList  [ M.singleton (Var "x") 1
                          , M.singleton (Var "x") 3 ]
  print $ show $   reconcile (S.fromList [x1_x2, x1_x3])
  assertBool "1" $ reconcile (S.fromList [x1_x2, x1_x3]) == x1

testReconcile1toMany = TestCase $ do
  let x1 = M.singleton (Var "x") 1
      y1 = M.singleton (Var "y") 1
      y2 = M.singleton (Var "y") 2
      x1y2 = M.fromList [ ((Var "x"),1)
                        , ((Var "y"),2)]
  assertBool "1" $ reconcile1toMany x1y2 (S.fromList [x1, y1, y2] )
    == S.singleton x1y2

testReconcile2 = TestCase $ do
  let x1 = M.singleton (Var "x") 1
      y1 = M.singleton (Var "y") 1
      y2 = M.singleton (Var "y") 2
      x1y2 = M.fromList [ ((Var "x"),1)
                        , ((Var "y"),2)]
  assertBool "0" $ reconcile2 M.empty M.empty == Just M.empty
  assertBool "1" $ reconcile2 M.empty x1      == Just x1
  assertBool "2" $ reconcile2 x1y2    M.empty == Just x1y2
  assertBool "3" $ reconcile2 x1      y2      == Just x1y2
  assertBool "3" $ reconcile2 y2      x1y2    == Just x1y2
  assertBool "3" $ reconcile2 y1      x1y2    == Nothing

testValidExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = VarFunc (Var "x") $ S.empty
      y   = VarFunc (Var "y") $ S.empty
      qx  = ForSome x qf
      qy  = ForSome y qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ disjointExistentials (ForSome x qx)  == False
  assertBool "2" $ disjointExistentials (ForSome y qx)  == True
  assertBool "3" $ disjointExistentials qxy             == True
  assertBool "3" $ disjointExistentials (QAnd [qx,qxy]) == False

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

testInvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet g == h
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet h == g