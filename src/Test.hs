{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Data.List hiding (find)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Test.Rslt.RProgram
import Test.TGraph
import Test.TInspect
import Test.TLeaf
import Test.TProgram
import Test.TQuery
import Test.TRslt
import Test.TSubst
import Test.TUtil


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleUtil
  , testModuleQueryClassify
  , testModuleGraph
  , test_module_Program
  , test_modules_leaf
  , test_module_query
  , test_module_rslt
  , test_module_rsltProgram
  , testModuleSubst
  ]
