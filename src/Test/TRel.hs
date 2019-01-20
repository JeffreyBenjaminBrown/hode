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

test_restrictToMatchIns = TestCase $ do
  let meh = error "whatever"
  assertBool "1" $ restrictToMatchIns
    (TSource [] ["a1","b1"] [])
    (TSourcePlan "what" "ever" ["a","b"] [])
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
    (TSourcePlan meh meh ["a","b"] meh)
    "a1" == Right "a"
