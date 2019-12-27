{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil (
    unEitherSt                   -- ^ Either String St -> St -> St
  , bufferRow_from_viewExprNode  -- ^       ViewExprNode -> ExprRow
  , bufferRow_from_viewExprNode' -- ^ St -> ViewExprNode
                                 --        -> Either String ExprRow
  , defaulViewOptions            -- ^ ViewOptions
  , emptySt                      -- ^ Rslt -> St
  , emptyBuffer                  -- ^ Buffer
  , emptyCycleBuffer            -- ^ Porest ExprRow
  , buffer_from_bufferRowTree    -- ^ PTree ViewExprNode
                                 -- -> Either String Buffer
  ) where

import qualified Data.List.PointedList as P
import           Data.Map (Map)
import qualified Data.Map              as M
import           Data.Set (Set)
import qualified Data.Set              as S
import           Lens.Micro

import qualified Brick.Focus           as B
import qualified Brick.Widgets.Edit    as B

import Hode.Brick
import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.PTree.Initial
import Hode.Qseq.QTypes (Var(..))
import Hode.Rslt.RTypes
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) =
  old & showError s
unEitherSt _ (Right new) =
  new & showingErrorWindow .~ False

defaulViewOptions :: ViewOptions
defaulViewOptions = ViewOptions
  { _viewOpt_ShowAddresses = True
  , _viewOpt_ShowAsAddresses = True
  , _viewOpt_WrapLength = 60 }

bufferRow_from_viewExprNode :: ViewExprNode -> ExprRow
bufferRow_from_viewExprNode n =
  ExprRow n mempty $ OtherProps False

bufferRow_from_viewExprNode'
  :: St -> ViewExprNode -> Either String ExprRow
bufferRow_from_viewExprNode' st n@(VExpr (ViewExpr a _ _)) =
  prefixLeft "bufferRow_from_viewExprNode':" $ do
  let r = st ^. appRslt
      hs = st ^. columnHExprs
      sub :: Map Var Addr =
        M.singleton VarRowNode a
  matches :: Map HExpr (Set Addr) <-
    let f h = (h, hExprToAddrs r sub h)
    in ifLefts_map $ M.fromList $ map f hs
  let matchCounts :: Map HExpr Int =
        M.map S.size matches
  Right $ ExprRow { _viewExprNode = n
                    , _columnProps = matchCounts
                    , _otherProps = OtherProps False }
bufferRow_from_viewExprNode' _ n =
  Right $ bufferRow_from_viewExprNode n

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _searchBuffers = Just $ porestLeaf emptyBuffer
                          & P.focus . pTreeHasFocus .~ True
  , _columnHExprs = [ HMember $ HVar VarRowNode ]
  , _blockingCycles = Nothing
  , _uiError     = ""
  , _reassurance = "This window provides reassurance. It's all good."
  , _commands    = B.editor (BrickOptionalName Commands)
                   Nothing ""
  , _commandHistory = []
  , _appRslt        = r
  , _viewOptions    = defaulViewOptions
  , _showingErrorWindow  = False
  , _showingInMainWindow = SearchBuffer
  , _showingOptionalWindows = M.fromList [ (Commands   , True)
                                         , (Reassurance, True) ]
  }

emptyBuffer :: Buffer
emptyBuffer = Buffer
  { _bufferQuery = QueryView "(empty buffer)"
  , _bufferRowPorest =
    Just $ porestLeaf $ bufferRow_from_viewExprNode $ VQuery $ QueryView
    "If you run a search, what it finds will be shown here."
  }

emptyCycleBuffer :: Buffer
emptyCycleBuffer = Buffer
  { _bufferQuery = CycleView
  , _bufferRowPorest = Just $
    porestLeaf $ bufferRow_from_viewExprNode $
    VQuery $ QueryView "There is no cycle to show here (yet)." }

-- | TODO : handle `VMember`s and `VCenterRole`s too.
buffer_from_bufferRowTree ::
  PTree ExprRow -> Either String Buffer
buffer_from_bufferRowTree ptbr =
  prefixLeft "buffer_from_bufferRowTree:" $ do
  let (br :: ExprRow) = ptbr ^. pTreeLabel
  ve :: ViewExpr <-
    case br ^. viewExprNode of
      VExpr x -> Right x
      _ -> Left $ "called from a non-VExpr."
  Right $ Buffer
    { _bufferQuery = QueryView $
                     unColorString $ ve ^. viewExpr_String
    , _bufferRowPorest = P.fromList [ptbr]
    }
