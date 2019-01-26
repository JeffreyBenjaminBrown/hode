{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Test.TLeaf where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Query.Inspect
import Query.MkLeaf
import Query.RunLeaf
import Space.Graph
import Space.Graph.GQuery
import Types


test_modules_leaf = TestList [
    TestLabel "test_runVarTest" test_runVarTest
  , TestLabel "test_runFind" test_runFind
  , TestLabel "test_runTest" test_runTest
  , TestLabel "test_mkVarCompare" test_mkVarCompare
  ]

test_mkVarCompare = TestCase $ do
  let (a,b) = ("a","b")
      t_34, t_ab :: VarTest Int (Graph Int)
      t_34 = mkVarCompare (/=) (Left 3)    (Left 4)
      t_ab = mkVarCompare (/=) (Right "a") (Right "b")
      a1b1 = M.fromList [(a,1),(b,1)]
      a1b2 = M.fromList [(a,1),(b,2)]
  assertBool "1" $ True  == varTestFunction t_34 (graph []) M.empty
  assertBool "2" $ False == varTestFunction t_ab (graph []) a1b1
  assertBool "3" $ True  == varTestFunction t_ab (graph []) a1b2

test_runTest = TestCase $ do
  -- I wrote tests for this function twice by accident. They are collected
  -- here, and probably redundant.
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      (a2 :: (Subst Int)) = M.singleton a 2
      (ce :: (CondElts Int)) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                    , (2, S.singleton $ M.empty) ]
  assertBool "1" $ runTest g a2 (mkTest (/=) $ Left 1) ce
    == M.singleton 2 (S.singleton M.empty)
  assertBool "2" $ runTest g a2 (mkTest (/=) $ Right a) ce
    == M.singleton 1 ( S.singleton $ M.fromList [(a,2), (x,0)] )

  let (a,b,c) = ("a","b","c")
      g = graph [ (1, [2,3] )
                , (2, [1,3] ) ]
      s = M.fromList [(a, 1), (b, 2)]
  assertBool "1" $ runTest g s
    ( mkTest (>) (Left 1) )
    (  M.fromList [ (0, S.singleton $ M.singleton c 1)
                  , (2, S.singleton $ M.singleton c 1) ] )
    == M.fromList [ (0, S.singleton $ M.singleton c 1) ]
  assertBool "2" $ runTest g s
    ( mkTest (<) (Right a) )
    (  M.fromList [ (0, S.singleton $ M.singleton c 1)
                  , (2, S.singleton $ M.singleton c 1) ] )
    == M.fromList [ (2, S.singleton $ M.fromList [ (a,1), (c,1) ] ) ]

test_runFind = TestCase $ do
  let (a,b,c) = ("a","b","c")
      g = graph [ (1, [2,3] ) ]
      s = M.fromList [(a, 1), (b, 2)]
  assertBool "1" $ runFind g s (findChildren $ Left 1)  ==
    M.fromList [ (2, S.singleton M.empty)
               , (3, S.singleton M.empty) ]
  assertBool "2" $ runFind g s (findChildren $ Right a) ==
    M.fromList [ (2, S.singleton $ M.singleton a 1)
               , (3, S.singleton $ M.singleton a 1) ]
  assertBool "2" $ runFind g s (findChildren $ Right b) == M.empty

test_runVarTest = TestCase $ do
  let a_lt_1 = mkVarCompare (>) (Left 1)    $ Right "a"
      b_lt_1 = mkVarCompare (>) (Left 1)    $ Right "b"
      a_gt_b = mkVarCompare (>) (Right "a") $ Right "b"
      b_gt_a = mkVarCompare (>) (Right "b") $ Right "a"
      subst = M.fromList [("a",0),("b",2)] :: Subst Int
  assertBool "1" $ True  == runVarTest (error "whatever") subst a_lt_1
  assertBool "2" $ False == runVarTest (error "whatever") subst b_lt_1
  assertBool "3" $ False == runVarTest (error "whatever") subst a_gt_b
  assertBool "4" $ True == runVarTest (error "whatever") subst b_gt_a
