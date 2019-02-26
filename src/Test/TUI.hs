{-# LANGUAGE ScopedTypeVariables #-}

module Test.TUI where

import qualified Test.HUnit as T
import           Test.HUnit hiding (Test, test)

import           Hash.HTypes
import           Rslt.RTypes
import           UI.IParse
import           UI.ITypes
--import qualified Test.Rslt.RData as D


test_module_ui :: T.Test
test_module_ui = TestList [
  TestLabel "test_pCommand_find" test_pCommand_find
  , TestLabel "test_pCommand_insert" test_pCommand_insert
  ]

test_pCommand_insert :: T.Test
test_pCommand_insert = TestCase $ do
  assertBool "1" $ pCommand_insert (error "irrelevant")
    "/add done told ya once"
    == Right ( CommandInsert $ Word "done told ya once" )

test_pCommand_find :: T.Test
test_pCommand_find = TestCase $ do
  assertBool "1" $ pCommand_find
    "/find done told ya once"
    == Right ( CommandFind $ HExpr $ Word "done told ya once" )
