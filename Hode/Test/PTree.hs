{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.PTree where

import Test.HUnit

import Hode.Test.PTree.TPInitial
import Hode.Test.PTree.TPModify
import Hode.Test.PTree.TPShow


test_modules_ptree :: Test
test_modules_ptree = TestList [
    test_module_pTree_show
  , test_module_pTree_initial
  , test_module_pTree_modify
  ]
