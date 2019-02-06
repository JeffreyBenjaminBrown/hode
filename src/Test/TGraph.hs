{-# LANGUAGE ScopedTypeVariables #-}

module Test.TGraph where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Data.Graph
import SeekSeq.STypes


testModuleGraph = TestList [
    TestLabel "test_InvertMapToSet" test_InvertMapToSet
  ]

test_InvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "1" $ invertMapToSet g == h
  assertBool "2" $ invertMapToSet h == g
