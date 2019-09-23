module Hode.Test.Main where

import           Test.HUnit hiding (Test)

import Hode.Test.Qseq.TLeaf
import Hode.Test.Qseq.TProgram
import Hode.Test.Qseq.TQuery
import Hode.Test.Qseq.TSubst
import Hode.Test.Qseq.TValid

import Hode.Test.TUtil
import Hode.Test.Hash.Main
import Hode.Test.Rslt.Main

--import Hode.Test.TBrick
import Hode.Test.TGraph
import Hode.Test.TPTree
import Hode.Test.TUI


tests :: IO Counts
tests = runTestTT $ TestList
  [ testModuleGraph
  , testModuleQueryClassify
  , testModuleSubst
  , test_module_Program
  , test_modules_leaf
  , test_module_query
--  , test_module_rsltProgram

  , test_modules_hode_hash
  , test_modules_hode_rslt
  , test_module_pTree

  , test_module_util

--  , test_module_hode_brick
  , test_module_ui
  ]
