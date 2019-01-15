{-# LANGUAGE ScopedTypeVariables #-}

module Test.TGraph where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Types


testModuleGraph = TestList [
    TestLabel "test_InvertMapToSet" test_InvertMapToSet
  , TestLabel "test_isNot_2" test_isNot_2
  ]

test_isNot_2 = TestCase $ do
  let (a,b) = ("a","b")
      t_34 = isNot_2 (Left 3)    (Left 4)    :: VarTest
      t_ab = isNot_2 (Right "a") (Right "b") :: VarTest
      a1b1 = M.fromList [(a,1),(b,1)]
      a1b2 = M.fromList [(a,1),(b,2)]
  assertBool "1" $ False == varTestFunction t_34 (graph []) M.empty 
  assertBool "2" $ True  == varTestFunction t_ab (graph []) a1b1
  assertBool "3" $ False == varTestFunction t_ab (graph []) a1b2

test_InvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "1" $ invertMapToSet g == h
  assertBool "2" $ invertMapToSet h == g
