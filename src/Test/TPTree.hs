{-# LANGUAGE ScopedTypeVariables #-}

module Test.TPTree where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as P

import           Util.PTree


test_module_pTree :: T.Test
test_module_pTree = TestList [
    TestLabel "test_porestLeaf" test_porestLeaf
  , TestLabel "test_focusedSubtree" test_focusedSubtree
  ]

test_porestLeaf :: T.Test
test_porestLeaf = TestCase $ do
  assertBool "1" $ Just (porestLeaf 1) == 
    ( P.fromList [ PTree { _pTreeLabel = 1 :: Int
                         , _pTreeFocused = False
                         , _pMTrees = Nothing } ] )

test_focusedSubtree :: T.Test
test_focusedSubtree = TestCase $ do
  let f = pTreeLeaf (1 :: Int)
      t = f { _pTreeFocused = True }
      ft = f { _pMTrees = Just $ P.singleton t }
      ff = f { _pMTrees = Just $ P.singleton f }
      tf = t { _pMTrees = Just $ P.singleton f }
  assertBool "1" $ focusedSubtree f  == Nothing
  assertBool "2" $ focusedSubtree t  == Just t
  assertBool "3" $ focusedSubtree ft == Just t
  assertBool "4" $ focusedSubtree ff == Nothing
  assertBool "5" $ focusedSubtree tf == Just tf
