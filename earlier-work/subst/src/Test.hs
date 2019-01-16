module Test where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Graph
import Search
import Subst
import Test.Data
import Types
import Util


tests :: [Bool]
tests = [ and testSearch
        , and testTestSolutions
        , and testUnifySolutions_2
        , and testUnifySolutions_1_to_many
        , and testUnifySolutions
        , and testUnifySubsts
        , and testPqSearch
        , and testNqTest
        , and testGraphChildren
        , and testGraphParents
        , and testSetUnions
        ]

testSearch :: [Bool]
testSearch =
  [ let s = search [1..10] ( QOr [ QP $ QElt 1
                                 , QP $ QElt 2 ] )
    in solved s == S.fromList [ (1, M.empty), (2, M.empty) ]
       && isNothing (unsolved s)
  , let s = search [1..10] ( QOr [ QP $ QElt 1
                                 , QV $ Var "a" ] )
        f (Just (QV (Var "a"))) = True -- need because Query is not Eq
        f _                     = False
    in solved s == S.singleton (1, M.empty)
       && f (unsolved s)
  , let s = search [1..10] ( QOr [ QOr [ QOr [ QP $ QElt 1 ] ] ] )
    in solved s == S.singleton (1, M.empty)
       && isNothing (unsolved s)
  , let s = search [1..10] ( QOr [ QOr [ QOr [ QV $ Var "a"
                                             , QP $ QElt 1 ] ] ] )
        f (Just (QV (Var "a"))) = True
        f _                     = False
    in solved s == S.singleton (1, M.empty)
       && f (unsolved s)
  ]

testTestSolutions :: [Bool]
testTestSolutions =
  [ testSolution [1] (1, M.empty)                 (QV $ Var "a")
    == S.singleton   (1, M.singleton (Var "a") 1)
  , testSolution [1] (1, M.singleton (Var "a") 1) (QV $ Var "a")
    == S.singleton   (1, M.singleton (Var "a") 1)
  , testSolution [1] (1, M.singleton (Var "a") 2) (QV $ Var "a")
    == S.empty

  , testSolution [1..9] (1, M.singleton (Var "a") 2) (QP $ QElt 1)
    == S.singleton      (1, M.singleton (Var "a") 2)
  , testSolution [1..9] (1, M.singleton (Var "a") 2) (QP $ QFind S.fromList)
    == S.singleton      (1, M.singleton (Var "a") 2)

  , testSolution [1..9] (1, M.singleton (Var "a") 1)
    (QN $ QNot $ QV $ Var "a")
    == S.empty
  , testSolution [1..9] (1, M.empty) (QN $ QCond $ \_ e -> e > 5)
    == S.empty
  , testSolution [1..9] (1, M.empty) (QN $ QCond $ \_ e -> e < 5)
    == S.singleton (1, M.empty)

  , testSolution [1..9] (1, M.empty) (QOr [ QN $ QCond $ \_ e -> e < 5
                                          , QV $ Var "a" ] )
    == S.fromList [ (1, M.empty)
                  , (1, M.singleton (Var "a") 1) ]

  , testSolution [1..9] (1, M.empty) (QAnd [ QN $ QCond $ \_ e -> e < 5
                                           , QV $ Var "a" ] )
    == S.fromList [ (1, M.singleton (Var "a") 1) ]
  ]

testUnifySolutions_2 :: [Bool]
testUnifySolutions_2 =
  [ unifySolutions_2 (1, M.empty) (1, M.empty) == Just (1, M.empty)
  , unifySolutions_2 (1, M.empty) (2, M.empty) == Nothing
  , unifySolutions_2 (1, M.singleton (Var "a") 1) (1, M.singleton (Var "a") 1)
             == Just (1, M.singleton (Var "a") 1)
  , unifySolutions_2 (1, M.singleton (Var "a") 1) (1, M.singleton (Var "b") 1)
             == Just (1, M.fromList [(Var "a", 1), (Var "b", 1)])
  , unifySolutions_2 (1, M.singleton (Var "a") 1) (1, M.singleton (Var "a") 2)
    == Nothing
  ]

testUnifySolutions_1_to_many :: [Bool]
testUnifySolutions_1_to_many =
  [ unifySolutions_1_to_many
                    (1, M.singleton (Var "a") 1)
      (S.fromList [ (1, M.singleton (Var "a") 1)
                  , (1, M.singleton (Var "b") 1) ] )
    == S.fromList [ (1, M.singleton (Var "a") 1)
                   , (1, M.fromList [ (Var "a", 1)
                                    , (Var "b", 1)
                                    ] ) ]
  , unifySolutions_1_to_many
                    (1, M.singleton (Var "a") 1)
      (S.fromList [ (1, M.singleton (Var "a") 2)
                  , (2, M.empty) ] )
    == S.empty
  ]

testUnifySolutions :: [Bool]
testUnifySolutions =
  [ unifySolutions ( S.fromList
                     [ S.fromList [ (1, M.singleton (Var "a") 1)
                                  , (2, M.empty) ]
                     , S.fromList [ (1, M.singleton (Var "a") 2)
                                  , (2, M.empty) ]
                     ] )
    == S.singleton (2, M.empty)
  ]

testUnifySubsts :: [Bool]
testUnifySubsts =
  [ unifySubsts_2 M.empty M.empty == Just (M.empty :: Subst Int)
  , unifySubsts_2 ( M.fromList [(Var "a",0), (Var "b",1)])
                  ( M.fromList [(Var "b",1), (Var "a",0)])
         == (Just $ M.fromList [(Var "b",1), (Var "a",0)])
  , unifySubsts_2 ( M.fromList [(Var "a",0), (Var "b",1)])
                  ( M.fromList [(Var "b",1)] )
         == (Just $ M.fromList [(Var "b",1), (Var "a",0)])
  , unifySubsts [] == Just (M.empty :: Subst Int)
  , unifySubsts [ M.singleton (Var "a") 0
                , M.singleton (Var "b") 1
                , M.singleton (Var "c") 2 ]
         == Just (M.fromList [(Var "a",0), (Var "b",1), (Var "c",2)])
  , unifySubsts [ M.singleton (Var "a") 0
                , M.empty
                , M.singleton (Var "a") 2 ]
    == Nothing
  ]

testPqSearch :: [Bool]
testPqSearch =
  [ pqSearch (S.fromList [1..10]) (QElt 1) == S.singleton 1
  , pqSearch (S.fromList [1..10]) (QElt 11) == S.empty
  , pqSearch (S.fromList [1..10])
    (QFind $ S.intersection $ S.fromList [6..15])
    == S.fromList [6..10]
  ]

testNqTest :: [Bool]
testNqTest =
  [ nqTest (S.fromList [1..10]) (QNot $ QP $ QElt 1  ) 2 == Right True
  , nqTest (S.fromList [1..10]) (QNot $ QP $ QElt 1  ) 1 == Right False
  , nqTest (S.fromList [1..10]) (QNot $ QV $ Var  "a") 1 == Left Pending

  , nqTest (S.fromList [1..10]) (QCond $ \_ e -> e > 5) 1  == Right False
  , nqTest (S.fromList [1..10]) (QCond $ \_ e -> e > 5) 11 == Right False
  , nqTest (S.fromList [1..10]) (QCond $ \_ e -> e > 5) 10 == Right True
  ]

testGraphChildren :: [Bool]
testGraphChildren = [ children g 1 == S.fromList [11,12]
                    ]

testGraphParents :: [Bool]
testGraphParents = [ parents g 1  == S.empty
                   , parents g 12 == S.fromList [1,2]
                   ]

testSetUnions :: [Bool]
testSetUnions =
  [ setUnions (S.fromList [S.empty, S.singleton 2, S.singleton 2])
    == S.fromList [2] ]
