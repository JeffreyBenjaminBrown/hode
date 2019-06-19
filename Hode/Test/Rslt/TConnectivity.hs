module Hode.Test.Rslt.TConnectivity where

import           Test.HUnit

import Hode.Rslt.Connectivity


test_module_rslt_connectivity :: Test
test_module_rslt_connectivity = TestList [
    TestLabel "test_reachable" test_reachable
    ]

test_reachable :: Test
test_reachable = TestCase $ do
  let meh = error ""
  assertBool "" $ reachable meh meh == Left "nope"
