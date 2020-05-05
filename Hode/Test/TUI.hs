{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.TUI where

import           Control.Lens
import qualified Data.Set              as S
import qualified Data.List.PointedList as P
import qualified Test.HUnit            as T
import           Test.HUnit hiding (Test, test)

import           Hode.Hash.Types
import           Hode.PTree.Initial
import           Hode.Rslt.Binary
import           Hode.Rslt.Index (mkRslt)
import           Hode.Rslt.Types
import           Hode.UI.ExprTree
import           Hode.UI.Input.Parse
import           Hode.UI.Main
import           Hode.UI.Types.Names
import           Hode.UI.Types.State
import           Hode.UI.Types.Views
import           Hode.UI.Util
import qualified Hode.Test.Rslt.RData as D


test_module_ui :: T.Test
test_module_ui = TestList [
    TestLabel "test_pLangCmd" test_pLangCmd
  , TestLabel "test_groupHostRels" test_groupHostRels
  , TestLabel "test_st_cycleBufferLenses" test_st_cycleBufferLenses
  ]

test_st_cycleBufferLenses :: T.Test
test_st_cycleBufferLenses = TestCase $ do
  let st = emptySt $ mkRslt mempty
      p :: Maybe (Porest ExprRow) = P.fromList
        [ pTreeLeaf $ ExprRow
          { _viewExprNode = VenFork $ ViewFork
            { _viewForkCenter = Nothing
            , _viewForkSortTplt = Nothing
            , _viewForkType = VFQuery $ QueryView "something" }
          , _numColumnProps = mempty
          , _boolProps = BoolProps
            { _inSortGroup = False
            , _selected = False }
          , _otherProps = OtherProps { _folded = False
                                     , _childSort = Nothing } } ]
      b = Buffer { _bufferExprRowTree = pTreeLeaf $
                   exprRow_from_viewExprNode $
                   VenFork . viewFork_fromViewForkType $
                   VFQuery $ QueryView "meh" }
      c_empty = Buffer { _bufferExprRowTree = pTreeLeaf $
                         exprRow_from_viewExprNode $
                         VenFork . viewFork_fromViewForkType $
                         VFQuery CycleView }
      c_something = c_empty & bufferExprRowTree . pMTrees .~ p

  assertBool "" $
    ( ( st { _searchBuffers =
             P.fromList $ map pTreeLeaf [b,c_empty,b]})
      ^. stGetTopLevelBuffer_byQuery CycleView ) == Just c_empty
  assertBool "" $
    _searchBuffers
    ( ( st { _searchBuffers =
               P.fromList $ map pTreeLeaf [b,c_empty,b]})
        & stSetTopLevelBuffer_byQuery CycleView .~ c_something )
    == P.fromList (map pTreeLeaf [b,c_something,b])

test_groupHostRels :: T.Test
test_groupHostRels = TestCase $ do
  assertBool "1" $ (S.fromList <$> groupHostRels D.big 2) == Right
    ( S.fromList
      [ ( ViewFork
          { _viewForkCenter = Just 2
          , _viewForkSortTplt = Nothing
          , _viewForkType = VFRelHosts $ RelHostGroup
            { _memberHostsRole = RoleInRel' $ RoleMember 1
            , _memberHostsTplt = Tplt Nothing [Phrase "0"] Nothing } }
        ,[9] )
      , ( ViewFork
          { _viewForkCenter = Just 2
          , _viewForkSortTplt = Nothing
          , _viewForkType = VFRelHosts $ RelHostGroup
            { _memberHostsRole = RoleInRel' $ RoleMember 2
            , _memberHostsTplt = Tplt Nothing [Phrase "0"] Nothing } }
        ,[7] ) ] )


test_pLangCmd :: T.Test
test_pLangCmd = TestCase $ do
  assertBool "add" $ pLangCmd (error "irrelevant")
    "/add done told ya once "
    == Right ( LangCmdInsert $ Phrase "done told ya once" )

  assertBool "find" $ let s = "done told ya once"
    in pLangCmd (error "irrelevant") ("/find " ++ s)
    == Right ( LangCmdFind s $ HExpr $ Phrase "done told ya once" )

  assertBool "load" $ pLangCmd (error "irrelevant")
    "/load somewhere/over/the/rainbow.exe"
    == Right ( LangCmdLoad "somewhere/over/the/rainbow.exe" )

  assertBool "save" $ pLangCmd (error "irrelevant")
    "/save somewhere/over/the/rainbow.exe"
    == Right ( LangCmdSave "somewhere/over/the/rainbow.exe" )

  assertBool "sort" $ pLangCmd D.rslt
    "/sortRight (/t /_ needs /_)"
    == Right ( LangCmdSort
               "sort RightEarlier: (/t /_ needs /_)"
               RightEarlier 4 )

  assertBool "sort" $ pLangCmd D.rslt
    "/sl /@ 4"
    == Right ( LangCmdSort
               "sort LeftEarlier: /@ 4"
               LeftEarlier 4 )
