{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Main where

import           Test.HUnit hiding (Test)

--import Hode.Test.Hash.TConvert
--import Hode.Test.Hash.THLookup
--import Hode.Test.Hash.TParse
--import Hode.Test.Hash.TTransitive
import Hode.Test.Qseq.TLeaf
import Hode.Test.Qseq.TProgram
import Hode.Test.Qseq.TQuery
import Hode.Test.Qseq.TSubst
import Hode.Test.Qseq.TValid

import Hode.Test.Rslt.Main

--import Hode.Test.TBrick
import Hode.Test.TGraph
import Hode.Test.TPTree
--import Hode.Test.TUI


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleGraph
  , testModuleQueryClassify
  , testModuleSubst
  , test_module_Program
  , test_modules_leaf
  , test_module_query
--  , test_module_rsltProgram

  , test_modules_hode_rslt
  , test_module_pTree

--  , test_module_hash_convert
--  , test_module_hash_parse
--  , test_module_hash_hlookup_transitive
--  , test_module_hode_brick

--  , test_module_rslt_hash
--  , test_module_ui
  ]
