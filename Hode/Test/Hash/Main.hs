module Hode.Test.Hash.Main where

import           Test.HUnit

import Hode.Test.Hash.TConvert
import Hode.Test.Hash.THLookup
import Hode.Test.Hash.TParse
import Hode.Test.Hash.TTransitive

test_modules_hode_hash :: Test
test_modules_hode_hash = TestList [
  test_module_hash_convert,
  test_module_hash_hlookup_transitive,
  test_module_hash_lookup,
  test_module_hash_parse
  ]
