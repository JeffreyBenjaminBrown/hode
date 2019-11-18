{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil (
    unEitherSt             -- ^ Either String St -> St -> St
  , emptySt                -- ^ Rslt -> St
  , emptyBuffer            -- ^ Buffer
  , buffer_from_bufferRowTree -- ^ PTree ViewExprNode
                              -- -> Either String Buffer
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
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc
import Hode.PTree.Initial


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
  , _columnHExprs = let hv = HVar VarRowNode in
      [ HOr -- Count the relationships something is in.
      -- TODO : This is an incomplete hack. In TODO.org,
      -- see section called (HExpr: add a symbol for "involves")
        [ HMap $ M.singleton (RoleInRel'   RoleTplt    ) hv
        , HMap $ M.singleton (RoleInRel' $ RoleMember 1) hv
        , HMap $ M.singleton (RoleInRel' $ RoleMember 2) hv
        , HMap $ M.singleton (RoleInRel' $ RoleMember 3) hv
        ] ]
  , _uiError     = ""
  , _reassurance = "This window provides reassurance. It's all good."
  , _commands    = B.editor (BrickOptionalName Commands)
                   Nothing ""
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
    "If you run a search, what it finds will be shown here." }

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
