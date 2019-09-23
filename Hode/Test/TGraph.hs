{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TGraph where

import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)
import qualified Test.HUnit     as T

import Hode.Data.Graph


test_module_graph :: T.Test
test_module_graph = TestList [
    TestLabel "test_InvertMapToSet" test_InvertMapToSet
  ]

test_InvertMapToSet :: T.Test
test_InvertMapToSet = TestCase $ do
  let g = M.fromList [ (1 :: Int , S.fromList [2,3] )
                     , (2        , S.fromList [1,3] ) ]
      h = M.fromList [ (1 :: Int , S.fromList [2  ] )
                     , (2        , S.fromList [1  ] )
                     , (3        , S.fromList [1,2] ) ]
  assertBool "1" $ invertMapToSet g == h
  assertBool "2" $ invertMapToSet h == g
