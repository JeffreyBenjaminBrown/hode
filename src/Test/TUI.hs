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
  TestLabel "test_pCommand" test_pCommand
  ]

test_pCommand :: T.Test
test_pCommand = TestCase $ do
  assertBool "1" $ pCommand (error "irrelevant")
    "/add done told ya once "
    == Right ( CommandInsert $ Word "done told ya once" )

  assertBool "1" $ pCommand (error "irrelevant")
    "/find done told ya once"
    == Right ( CommandFind $ HExpr $ Word "done told ya once" )

  assertBool "1" $ pCommand (error "irrelevant")
    "/load somewhere/over/the/rainbow.exe"
    == Right ( CommandLoad "somewhere/over/the/rainbow.exe" )

  assertBool "1" $ pCommand (error "irrelevant")
    "/save somewhere/over/the/rainbow.exe"
    == Right ( CommandSave "somewhere/over/the/rainbow.exe" )
