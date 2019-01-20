{-# LANGUAGE ScopedTypeVariables #-}
module Test.TRel where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Space.Graph
import Program
import Query
import RelExperim
import Types


test_module_Rel = TestList [
  TestLabel "test_renameIn" test_renameIn
  , TestLabel "test_restrictToMatchIns" test_restrictToMatchIns
  ]

thinking = let (a, b, c, x, y, z)  = ("a", "b", "c", "x", "y", "z")
               (a1,b1,c1,x1,y1,z1) = ("a1","b1","c1","x1","y1","z1")
               (a2,b2,c2,x2,y2,z2) = ("a2","b2","c2","x2","y2","z2")
  in varPossibilities -- Possible e -> Subst e -> TSource
  ( M.fromList [ (x, ( M.fromList
                       [ ( 21, ( S.singleton
                                 $ M.fromList [(a,1), (b,11)] ) ) ] ) )
               , (y, ( M.fromList
                       [ ( 31, ( S.singleton
                                 $ M.fromList [(a,1), (b,11)] ) ) ] ) ) ] )
  ( M.fromList [(x1,21),(y1,31)] :: Subst Int)
  ( TSource
   [ ( TVarPlan a a1 [x1,y1] [] ) -- source, name, inputNames, outputNames
   , ( TVarPlan b b1 [x2,y2] [] ) ]
   []                                -- inputs
   [x1, y1] )                        -- outputs

test_restrictToMatchIns = TestCase $ do
  let meh = error "whatever"
  assertBool "1" $ restrictToMatchIns
    (TSource [] ["a1","b1"] [])   -- most args don't matter here
    (TVarPlan "" "" ["a","b"] []) -- most args don't matter here
    (M.fromList [("a1",1),("b1",1)] :: Subst Int)
    (M.fromList [ ( 1, S.fromList [ M.fromList [("a",1),("b",1),("c",1)]
                                  , M.fromList [("a",2),("b",2)] ] )
                , ( 2, S.fromList [ M.fromList [("a",2),("b",2)] ] ) ]
      :: CondElts Int)
    == Right ( M.fromList [ ( 1, S.singleton
                              $ M.fromList [("a",1),("b",1),("c",1)] ) ] )

test_renameIn = TestCase $ do
  let meh = error "whatev"
  assertBool "1" $ renameIn
    (TSource meh ["a1","b1"] meh)
    (TVarPlan meh meh ["a","b"] meh)
    "a1" == Right "a"
