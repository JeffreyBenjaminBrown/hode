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
  TestLabel "test_renamein" test_renamein
  ]

test_renamein = TestCase $ do
  let meh = error "whatev"
  assertBool "1" $ renameIn
    (TSource meh ["a1","b1"] meh)
    (TSourcePlan meh meh ["a","b"] meh)
    "a1" == Right "a"
