{-# LANGUAGE ScopedTypeVariables #-}

module Test.TUI where

import qualified Data.Set        as S
import qualified Data.Vector     as V
import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Hash.HTypes
import           Rslt.RTypes
import           UI.IParse
import           UI.ITypes
import           UI.IUtil
import           UI.ViewTree
import qualified Test.Rslt.RData as D



aViewTree :: ViewTree
aViewTree = ViewTree {
    _viewChildFocus = 0
  , _viewIsFocused = False
  , _viewContent = VQuery "top"
  , _viewSubviews = V.fromList
    $ map (viewLeaf . VQuery .show) [0,1 :: Int] }


test_module_ui :: T.Test
test_module_ui = TestList [
    TestLabel "test_pCommand" test_pCommand
  , TestLabel "test_groupHostRels" test_groupHostRels
  ]

test_groupHostRels :: T.Test
test_groupHostRels = TestCase $ do
  assertBool "1" $ (S.fromList <$> groupHostRels D.big 2) == Right
    ( S.fromList
      [ ( ViewCenterRole
          { _crvCenter = 2
          , _crvRole = RoleMember 1
          , _crvTplt = [Phrase "0",Phrase "0",Phrase "0"] }
        ,[9] )
      , ( ViewCenterRole
          { _crvCenter = 2
          , _crvRole = RoleMember 2
          , _crvTplt = [Phrase "0",Phrase "0",Phrase "0"] }
        ,[7] ) ] )

test_pCommand :: T.Test
test_pCommand = TestCase $ do
  assertBool "1" $ pCommand (error "irrelevant")
    "/add done told ya once "
    == Right ( CommandInsert $ Phrase "done told ya once" )

  assertBool "1" $ let s = "done told ya once"
    in pCommand (error "irrelevant") ("/find " ++ s)
    == Right ( CommandFind s $ HExpr $ Phrase "done told ya once" )

  assertBool "1" $ pCommand (error "irrelevant")
    "/load somewhere/over/the/rainbow.exe"
    == Right ( CommandLoad "somewhere/over/the/rainbow.exe" )

  assertBool "1" $ pCommand (error "irrelevant")
    "/save somewhere/over/the/rainbow.exe"
    == Right ( CommandSave "somewhere/over/the/rainbow.exe" )
