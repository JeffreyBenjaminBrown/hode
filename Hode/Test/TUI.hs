{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TUI where

import qualified Data.Set        as S
import qualified Data.Vector     as V
import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Hode.Hash.HTypes
import           Hode.Rslt.RTypes
import           Hode.UI.Input.IParse
import           Hode.UI.ITypes
import           Hode.UI.BufferRowTree
import           Hode.Util.VTree
import qualified Hode.Test.Rslt.RData as D


aViewExprNodeTree :: VTree ViewExprNode
aViewExprNodeTree = VTree {
    _vTreeFocalChild = 0
  , _vTreeIsFocused = False
  , _vTreeLabel = VQuery "top"
  , _vTrees = V.fromList
    $ map (vTreeLeaf . VQuery .show) [0,1 :: Int] }


test_module_ui :: T.Test
test_module_ui = TestList [
    TestLabel "test_pCommand" test_pCommand
  , TestLabel "test_groupHostRels" test_groupHostRels
  ]

test_groupHostRels :: T.Test
test_groupHostRels = TestCase $ do
  assertBool "1" $ (S.fromList <$> groupHostRels D.big 2) == Right
    ( S.fromList
      [ ( RelHostGroup MemberHosts
          { _memberHostsCenter = 2
          , _memberHostsRole = RoleMember 1
          , _memberHostsTplt = Tplt Nothing [Phrase "0"] Nothing }
        ,[9] )
      , ( RelHostGroup MemberHosts
          { _memberHostsCenter = 2
          , _memberHostsRole = RoleMember 2
          , _memberHostsTplt = Tplt Nothing [Phrase "0"] Nothing }
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
