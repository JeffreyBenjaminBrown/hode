{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.Main where

import           Test.HUnit as T

import Hode.Test.Rslt.RData
import Hode.Test.Rslt.RProgram
import Hode.Test.Rslt.TEdit
import Hode.Test.Rslt.TIndex_and_Valid
import Hode.Test.Rslt.TLookup
import Hode.Test.Rslt.TShow

test_modules_hode_rslt :: T.Test
test_modules_hode_rslt = TestList [
    test_the_rslt_test_data
  , test_module_rslt_index_and_valid
  , test_module_rslt_show
  , test_module_rslt_edit
  , test_module_rslt_exprToAddr
  ]
