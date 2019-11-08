{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil (
    unEitherSt             -- ^ Either String St -> St -> St

  , emptySt                -- ^ Rslt -> St
  , emptyBuffer            -- ^                                 Buffer
  , buffer_from_bufferRowTree -- ^ PTree ViewExprNode -> Either String Buffer
  , mkBufferRowPorest -- ^ Rslt -> [Addr] -> Porest BufferRow
  ) where

import qualified Data.List.PointedList as P
import qualified Data.Map              as M
import           Lens.Micro

import qualified Brick.Focus           as B
import qualified Brick.Widgets.Edit    as B

import Hode.Brick
import Hode.Hash.HTypes
import Hode.Rslt.RTypes
import Hode.Qseq.QTypes (Var(..))
import Hode.UI.IUtil.String (mkViewExpr)
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.PTree


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old & showError s
unEitherSt _ (Right new) = new & showingErrorWindow .~ False

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _searchBuffers = Just $ porestLeaf emptyBuffer
                          & P.focus . pTreeHasFocus .~ True
  , _columnHExprs = -- TODO : This is a hack. In TODO.org,
      -- see the section called  (HExpr: add a symbol for "involves")
      [ HOr [ HMap $ M.singleton (RoleInRel'   RoleTplt    ) $ HVar VarRowNode
            , HMap $ M.singleton (RoleInRel' $ RoleMember 1) $ HVar VarRowNode
            , HMap $ M.singleton (RoleInRel' $ RoleMember 2) $ HVar VarRowNode
            , HMap $ M.singleton (RoleInRel' $ RoleMember 3) $ HVar VarRowNode
            ] ]
  , _uiError   = ""
  , _reassurance = "It's all good."
  , _commands  = B.editor (BrickOptionalName Commands) Nothing ""
  , _commandHistory = []
  , _appRslt   = r
  , _showingErrorWindow = False
  , _showingInMainWindow = Results
  , _showingOptionalWindows = M.fromList [ (Commands   , True)
                                         , (Reassurance, True) ]
  }

emptyBuffer :: Buffer
emptyBuffer = Buffer {
    _bufferQuery = "(empty buffer)"
  , _bufferRowPorest =
    Just $ porestLeaf $ bufferRow_from_viewExprNode $ VQuery
    "There are no search results to show here (yet)." }

-- | TODO : This ought to handle `VMember`s and `VCenterRole`s too.
buffer_from_bufferRowTree :: PTree BufferRow -> Either String Buffer
buffer_from_bufferRowTree vt = do
  let (br :: BufferRow) = vt ^. pTreeLabel
  vr :: ViewExpr <- case br ^. viewExprNode of
    VExpr x -> Right x
    _ -> Left $ "buffer_from_bufferRowTree called from a non-VExpr."
  Right $ Buffer {
      _bufferQuery     = unAttrString $ vr ^. viewExpr_String
    , _bufferRowPorest = P.fromList [vt]
    }

mkBufferRowPorest :: Rslt -> [Addr] -> Porest BufferRow
mkBufferRowPorest r as =
  maybe ( porestLeaf $ bufferRow_from_viewExprNode $
          VQuery "No matches found.") id $
  P.fromList $ map v_qr as
  where
    v_qr :: Addr -> PTree BufferRow
    v_qr a = pTreeLeaf $ bufferRow_from_viewExprNode $
             VExpr $ either err id rv
      where
        (rv :: Either String ViewExpr) = mkViewExpr r a
        (err :: String -> ViewExpr) = \se -> error (", called on Find: should be impossible: `a` should be present, as it was just found by `hExprToAddrs`, but here's the original error: " ++ se)

