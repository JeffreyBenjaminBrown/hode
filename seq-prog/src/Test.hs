{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Test.TInspect
import Test.TGraph
import Test.TProgram
import Test.TQuery
import Test.TRslt
import Test.TSubst
import Test.TUtil


import Query.Inspect
import Types


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleUtil
  , testModuleQueryClassify
  , testModuleGraph
  , test_module_Program
  , testModuleQuery
  , test_module_rslt
  , testModuleSubst
  ]
