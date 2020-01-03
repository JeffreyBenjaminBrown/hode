{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.Main where

import Test.HUnit

import Hode.Test.PTree.TPInitial
import Hode.Test.PTree.TPModify
import Hode.Test.PTree.TPShow


test_modules_hode_ptree :: Test
test_modules_hode_ptree = TestList [
    test_module_pTree_show
  , test_module_pTree_initial
  , test_module_pTree_modify
  ]
