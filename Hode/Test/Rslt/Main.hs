{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.Main where

import Test.HUnit

import Hode.Test.Rslt.RData
import Hode.Test.Rslt.RProgram
import Hode.Test.Rslt.TEdit
import Hode.Test.Rslt.TIndex
import Hode.Test.Rslt.TLookup
import Hode.Test.Rslt.TShow
import Hode.Test.Rslt.TValid


test_modules_hode_rslt :: Test
test_modules_hode_rslt = TestList [
    test_the_rslt_test_data
  , test_module_rsltProgram
  , test_module_rslt_edit
  , test_module_rslt_exprToAddr
  , test_module_rslt_index
  , test_module_rslt_show
  , test_module_rslt_valid
  ]
