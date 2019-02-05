{-# LANGUAGE ScopedTypeVariables #-}

module Test.TMain where

import           Data.List hiding (find)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Test.Rslt.RProgram
import Test.Rslt.TEdit
import Test.Rslt.THash
import Test.Rslt.TIndex_and_Valid
import Test.Rslt.TLookup
import Test.Rslt.TShow
import Test.SeekSeq.TLeaf
import Test.SeekSeq.TProgram
import Test.SeekSeq.TQuery
import Test.SeekSeq.TSubst
import Test.SeekSeq.TValid
import Test.TGraph
import Test.TUtil


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleGraph
  , testModuleQueryClassify
  , testModuleSubst
  , testModuleUtil
  , test_module_Program
  , test_module_query
  , test_module_rsltProgram
  , test_module_rslt_edit
  , test_module_rslt_hash
  , test_module_rslt_index_and_valid
  , test_module_rslt_lookup
  , test_module_rslt_show
  , test_modules_leaf
  ]
