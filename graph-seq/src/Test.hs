{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Test.HUnit

import Test.TGraph
import Test.TQuery
import Test.TSubst
import Test.TUtil


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleUtil
  , testModuleGraph
  , testModuleQuery
  , testModuleSubst
  ]
