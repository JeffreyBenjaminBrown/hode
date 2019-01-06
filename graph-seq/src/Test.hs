module Test where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Types


tests = runTestTT $ TestList
  [ TestLabel "graphTest" graphTest ]

graphTest = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet g == h
  assertBool "invert a Map from a to (Set a), 1" $ invertMapToSet h == g
