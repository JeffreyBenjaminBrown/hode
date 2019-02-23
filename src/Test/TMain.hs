{-# LANGUAGE ScopedTypeVariables #-}

module Test.TMain where

import           Test.HUnit hiding (Test)

import Test.Rslt.RData
import Test.Rslt.RProgram
import Test.Rslt.TEdit
import Test.Rslt.THash
import Test.Rslt.TIndex_and_Valid
import Test.Rslt.TLookup
import Test.Rslt.TShow
import Test.Qseq.TLeaf
import Test.Qseq.TProgram
import Test.Qseq.TQuery
import Test.Qseq.TSubst
import Test.Qseq.TValid
import Test.TGraph
import Test.Hash.TConvert
import Test.Hash.TParse


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleGraph
  , testModuleQueryClassify
  , testModuleSubst
  , test_module_Program
  , test_module_query
  , test_the_rslt_test_data
  , test_module_rsltProgram
  , test_module_rslt_edit
  , test_module_rslt_hash
  , test_module_rslt_index_and_valid
  , test_module_rslt_exprToAddr
  , test_module_rslt_show
  , test_modules_leaf
  , test_module_hash_convert
  , test_module_hash_parse
  ]
