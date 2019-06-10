{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TMain where

import           Test.HUnit hiding (Test)

import Hode.Test.Hash.TConvert
import Hode.Test.Hash.THLookup
import Hode.Test.Hash.TParse
import Hode.Test.Qseq.TLeaf
import Hode.Test.Qseq.TProgram
import Hode.Test.Qseq.TQuery
import Hode.Test.Qseq.TSubst
import Hode.Test.Qseq.TValid
import Hode.Test.Rslt.RData
import Hode.Test.Rslt.RProgram
import Hode.Test.Rslt.TEdit
import Hode.Test.Rslt.TIndex_and_Valid
import Hode.Test.Rslt.TLookup
import Hode.Test.Rslt.TShow
import Hode.Test.TBrick
import Hode.Test.TGraph
import Hode.Test.TPTree
import Hode.Test.TUI


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
  , test_module_pTree
  , test_module_ui
  , test_module_hode_brick
  ]
