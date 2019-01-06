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
--  , TestLabel "testFindable" testFindable
  ]

--testFindable = TestCase $ do
--  let qf = QFind $ \_ _    -> S.empty
--      qc = QCond $ \_ _  _ -> False
--      label = "testFindable"
--  assertBool label $ (findable $ QIntersect (S.fromList [qf, qc])) == True
--  assertBool label $ (findable $ QIntersect (S.fromList [qc])    ) == False
--  assertBool label $ (findable $ QIntersect (S.fromList [])      ) == False
--  assertBool label $ (findable $ QUnion     (S.fromList [qf, qc])) == False
--  assertBool label $ (findable $ QUnion     (S.fromList [qc])    ) == True
--  assertBool label $ (findable $ QUnion     (S.fromList [])      ) == True

--  , TestLabel "testValid" testValid
--testValid = TestCase $ do
--  let qx = ForAll (S.singleton $ Var "x") $ 
--  assertBool "testValid" $ True ==
--    (valid $ 

testInvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet g == h
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet h == g
