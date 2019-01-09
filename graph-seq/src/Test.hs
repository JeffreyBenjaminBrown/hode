{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Query
import Subst
import Types
import Util

import Test.TGraph
import Test.TQuery
import Test.TSubst
import Test.TUtil


tests = runTestTT $ TestList
  [ testModuleUtil
  , testModuleGraph
  , testModuleQuery
  , testModuleSubst
  ]
