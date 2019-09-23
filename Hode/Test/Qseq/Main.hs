{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Qseq.Main where

import Test.HUnit

import Hode.Test.Qseq.TLeaf
import Hode.Test.Qseq.TProgram
import Hode.Test.Qseq.TQuery
import Hode.Test.Qseq.TSubst
import Hode.Test.Qseq.TValid

test_modules_hode_qseq :: Test
test_modules_hode_qseq = TestList
  [ test_module_query_classify
  , test_module_subst
  , test_module_Program
  , test_modules_leaf
  , test_module_query
  ]
