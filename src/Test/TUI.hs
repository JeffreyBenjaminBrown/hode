{-# LANGUAGE ScopedTypeVariables #-}

module Test.TUI where

import qualified Test.HUnit as T
import           Test.HUnit hiding (Test, test)

import           Rslt.RTypes
import           UI.IParse
import           UI.ITypes
--import qualified Test.Rslt.RData as D


test_module_ui :: T.Test
test_module_ui = TestList [
  TestLabel "test_pInsertCommand" test_pInsertCommand
  ]

test_pInsertCommand :: T.Test
test_pInsertCommand = TestCase $ do
  assertBool "1" $ pInsertCommand (error "irrelevant")
    "/add done told ya once"
    == Right ( Insert $ Word "done told ya once" )
