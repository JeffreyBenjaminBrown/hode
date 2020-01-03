module Hode.Test.Main where

import           Test.HUnit hiding (Test)

import Hode.Test.TUtil

import Hode.Test.Hash.Main
import Hode.Test.Rslt.Main
import Hode.Test.Qseq.Main

import Hode.Test.TUI
import Hode.Test.TNoUI

import Hode.Test.TBrick
import Hode.Test.TGraph
import Hode.Test.PTree


tests :: IO Counts
tests = runTestTT $ TestList
  [ test_modules_hode_hash
  , test_modules_hode_rslt
  , test_modules_hode_qseq
  , test_modules_ptree

  , test_module_ui
  , test_module_NoUI
  , test_module_util
  , test_module_graph
  , test_module_hode_brick
  ]
