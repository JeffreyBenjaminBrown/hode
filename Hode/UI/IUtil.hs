{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil (
    unEitherSt             -- ^ Either String St -> St -> St
  , emptySt                -- ^ Rslt -> St
  , emptyBuffer            -- ^ Buffer
  , buffer_from_bufferRowTree -- ^ PTree ViewExprNode
                              -- -> Either String Buffer
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
import Hode.Util.Misc
import Hode.Util.PTree


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) =
  old & showError s
unEitherSt _ (Right new) =
  new & showingErrorWindow .~ False

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _searchBuffers = Just $ porestLeaf emptyBuffer
                          & P.focus . pTreeHasFocus .~ True
  , _columnHExprs =
      [ HOr -- Count the relationships something is in.
      -- TODO : This is an incomplete hack. In TODO.org,
      -- see the section called (HExpr: add a symbol for "involves")
        [ HMap $ M.singleton (RoleInRel'   RoleTplt    ) $ HVar VarRowNode
        , HMap $ M.singleton (RoleInRel' $ RoleMember 1) $ HVar VarRowNode
        , HMap $ M.singleton (RoleInRel' $ RoleMember 2) $ HVar VarRowNode
        , HMap $ M.singleton (RoleInRel' $ RoleMember 3) $ HVar VarRowNode
        ] ]
  , _uiError     = ""
  , _reassurance = "This window provides reassurance. It's all good."
  , _commands    = B.editor
    (BrickOptionalName Commands) Nothing ""
  , _commandHistory = []
  , _appRslt        = r
  , _showingErrorWindow  = False
  , _showingInMainWindow = Results
  , _showingOptionalWindows = M.fromList [ (Commands   , True)
                                         , (Reassurance, True) ]
  }

emptyBuffer :: Buffer
emptyBuffer = Buffer
  { _bufferQuery = "(empty buffer)"
  , _bufferRowPorest =
    Just $ porestLeaf $ bufferRow_from_viewExprNode $ VQuery
    "If you run a search, what it finds will be shown here" }

-- | TODO : handle `VMember`s and `VCenterRole`s too.
buffer_from_bufferRowTree ::
  PTree BufferRow -> Either String Buffer
buffer_from_bufferRowTree ptbr =
  prefixLeft "buffer_from_bufferRowTree:" $ do
  let (br :: BufferRow) = ptbr ^. pTreeLabel
  ve :: ViewExpr <-
    case br ^. viewExprNode of
      VExpr x -> Right x
      _ -> Left $ "called from a non-VExpr."
  Right $ Buffer
    { _bufferQuery = unColorString $ ve ^. viewExpr_String
    , _bufferRowPorest = P.fromList [ptbr]
    }

-- | Creates a depth-1 forest, i.e. with nothing but leaves.
mkBufferRowPorest :: Rslt -> [Addr] -> Porest BufferRow
mkBufferRowPorest r as =
  maybe ( porestLeaf $ bufferRow_from_viewExprNode $
          VQuery "No matches found.") id $
  P.fromList $ map mkLeaf as
  where
    mkLeaf :: Addr -> PTree BufferRow
    mkLeaf a =
      pTreeLeaf $ bufferRow_from_viewExprNode $
      VExpr $ either err id $ mkViewExpr r a
    err :: String -> ViewExpr
    err s = error $ ", called on Find: should be impossible: `a` should be present, as it was just found by `hExprToAddrs`, but here's the original error: " ++ s
