{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TUI where

import qualified Data.Set        as S
import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import           Hode.Hash.HTypes
import           Hode.Rslt.Binary
import           Hode.Rslt.RTypes
import           Hode.UI.BufferRowTree
import           Hode.UI.Types.Names
import           Hode.UI.Types.Views
import           Hode.UI.Input.IParse
import qualified Hode.Test.Rslt.RData as D


test_module_ui :: T.Test
test_module_ui = TestList [
    TestLabel "test_pCommand" test_pCommand
  , TestLabel "test_groupHostRels" test_groupHostRels
  ]

test_groupHostRels :: T.Test
test_groupHostRels = TestCase $ do
  assertBool "1" $ (S.fromList <$> groupHostRels D.big 2) == Right
    ( S.fromList
      [ ( RelHostFork RelHosts
          { _memberHostsCenter = 2
          , _memberHostsRole = RoleInRel' $ RoleMember 1
          , _memberHostsTplt = Tplt Nothing [Phrase "0"] Nothing }
        ,[9] )
      , ( RelHostFork RelHosts
          { _memberHostsCenter = 2
          , _memberHostsRole = RoleInRel' $ RoleMember 2
          , _memberHostsTplt = Tplt Nothing [Phrase "0"] Nothing }
        ,[7] ) ] )

test_pCommand :: T.Test
test_pCommand = TestCase $ do
  assertBool "add" $ pCommand (error "irrelevant")
    "/add done told ya once "
    == Right ( CommandInsert $ Phrase "done told ya once" )

  assertBool "find" $ let s = "done told ya once"
    in pCommand (error "irrelevant") ("/find " ++ s)
    == Right ( CommandFind s $ HExpr $ Phrase "done told ya once" )

  assertBool "load" $ pCommand (error "irrelevant")
    "/load somewhere/over/the/rainbow.exe"
    == Right ( CommandLoad "somewhere/over/the/rainbow.exe" )

  assertBool "save" $ pCommand (error "irrelevant")
    "/save somewhere/over/the/rainbow.exe"
    == Right ( CommandSave "somewhere/over/the/rainbow.exe" )

  assertBool "sort" $ pCommand D.rslt
    "/sortLeft (/t /_ needs /_) bottles"
    == Right ( CommandFindSort
               "sort LeftFirst: (/t /_ needs /_) bottles"
               (HExpr $ Phrase "bottles")
               LeftFirst 4 )

  assertBool "sort" $ pCommand D.rslt
    "/sr /@ 4 bottles"
    == Right ( CommandFindSort
               "sort RightFirst: /@ 4 bottles"
               (HExpr $ Phrase "bottles")
               RightFirst 4 )
