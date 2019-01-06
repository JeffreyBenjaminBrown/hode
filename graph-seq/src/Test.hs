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
      qxy = QIntersect [qx,qy]
  assertBool "1" $ validExistentials (ForSome x qx)        == False
  assertBool "2" $ validExistentials (ForSome y qx)        == True
  assertBool "3" $ validExistentials qxy                   == True
  assertBool "3" $ validExistentials (QIntersect [qx,qxy]) == False

testFindable = TestCase $ do
  let qf = QFind $ \_ _    -> S.empty
      qc = QCond $ \_ _  _ -> False
  assertBool "1" $ findable (QIntersect [qf, qc]) == True
  assertBool "2" $ findable (QIntersect [qf]    ) == True
  assertBool "3" $ findable (QIntersect [qc]    ) == False
  assertBool "4" $ findable (QIntersect []      ) == False
  assertBool "5" $ findable (QUnion     [qf, qc]) == False
  assertBool "6" $ findable (QUnion     [qf]    ) == True
  assertBool "7" $ findable (QUnion     [qc]    ) == False
  assertBool "8" $ findable (QUnion     []      ) == False

testInvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet g == h
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet h == g
