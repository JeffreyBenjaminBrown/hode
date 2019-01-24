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
import Types


--testModuleQuery = TestList [
--    TestLabel "
--  ]
--
--test_isNot_2 = TestCase $ do
--
