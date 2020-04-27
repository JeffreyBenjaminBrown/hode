{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Util (
    unEitherSt                 -- ^ Either String St -> St -> St
  , emptySt                    -- ^ Rslt -> St
  , emptySubgraphBuffer          -- ^ Buffer
  , bufferFrom_viewQuery       -- ^ ViewQuery -> Buffer
  , buffer_from_exprRowTree    -- ^ PTree ViewExprNode
                               -- -> Either String Buffer
  , viewFork_fromViewForkType  -- ^ ViewForkType -> ViewFork
  , exprRow_fromQuery          -- ^ ViewQuery -> ExprRow
  , exprRow_from_viewExprNode' -- ^ St -> ViewExprNode
                               --        -> Either String ExprRow
  , exprRow_from_viewExprNode  -- ^       ViewExprNode -> ExprRow
  , defaulViewOptions          -- ^ ViewOptions
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
import Hode.Hash.Lookup
import Hode.Hash.Types
import Hode.PTree.Initial
import Hode.Qseq.Types (Var(..))
import Hode.Rslt.Types
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) =
  old & showError s
unEitherSt _ (Right new) =
  new & showingOptionalWindows %~ S.delete Error

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName LangCmds]
  , _searchBuffers = Just $ porestLeaf emptySubgraphBuffer
                          & P.focus . pTreeHasFocus .~ True
  , _columnHExprs = [ HMemberHosts $ HVar VarRowNode ]
  , _blockingCycles = Nothing
  , _uiError     = ""
  , _reassurance = "This window provides reassurance. It's all good."
  , _commands    = B.editor (BrickOptionalName LangCmds)
                   Nothing ""
  , _commandHistory = []
  , _appRslt        = r
  , _viewOptions    = defaulViewOptions
  , _showingInMainWindow = SubgraphBuffer
  , _showingOptionalWindows = S.fromList [ LangCmds, Reassurance ]
  }

emptySubgraphBuffer :: Buffer
emptySubgraphBuffer =
  bufferFrom_viewQuery $ QueryView $ "Empty search buffer. "
  ++ "(If you run a search, the results will be shown here.)"

bufferFrom_viewQuery :: ViewQuery -> Buffer
bufferFrom_viewQuery vq = Buffer
  { _bufferExprRowTree =
    pTreeLeaf $ exprRow_fromQuery vq
  }

-- | TODO : handle `VMember`s and `VCenterRole`s too.
buffer_from_exprRowTree ::
  PTree ExprRow -> Either String Buffer
buffer_from_exprRowTree ptbr =
  prefixLeft "buffer_from_exprRowTree:" $ do
  let (br :: ExprRow) = ptbr ^. pTreeLabel
  ve :: ViewExpr <-
    case br ^. viewExprNode of
      VenExpr x -> Right x
      _ -> Left $ "called from a non-VenExpr."
  Right $ Buffer
    { _bufferExprRowTree = PTree
      { _pTreeLabel =
        exprRow_fromQuery . QueryView $
        unColorString $ ve ^. viewExpr_String
      , _pTreeHasFocus = False
      , _pMTrees = P.fromList [ptbr]
      } }

viewFork_fromViewForkType :: ViewForkType -> ViewFork
viewFork_fromViewForkType vft = ViewFork
  { _viewForkCenter = Nothing
  , _viewForkSortTplt = Nothing
  , _viewForkType = vft }

exprRow_fromQuery :: ViewQuery -> ExprRow
exprRow_fromQuery =
  exprRow_from_viewExprNode . VenFork .
  viewFork_fromViewForkType . VFQuery

exprRow_from_viewExprNode'
  :: St -> ViewExprNode -> Either String ExprRow
exprRow_from_viewExprNode' st n@(VenExpr (ViewExpr a _ _)) =
  prefixLeft "exprRow_from_viewExprNode':" $ do
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
                  , _numColumnProps = matchCounts
                  , _boolProps = BoolProps
                    { _inSortGroup = False
                    , _selected = False }
                  , _otherProps = OtherProps { _folded = False
                                             , _childSort = Nothing } }
exprRow_from_viewExprNode' _ n =
  Right $ exprRow_from_viewExprNode n

exprRow_from_viewExprNode :: ViewExprNode -> ExprRow
exprRow_from_viewExprNode n = ExprRow
  { _viewExprNode = n
  , _numColumnProps = mempty
  , _boolProps = BoolProps
      { _inSortGroup = False
      , _selected = False }
  , _otherProps = OtherProps { _folded = False
                             , _childSort = Nothing } }

defaulViewOptions :: ViewOptions
defaulViewOptions = ViewOptions
  { _viewOpt_ShowAddresses = False
  , _viewOpt_ShowAsAddresses = False
  , _viewOpt_WrapLength = 60 }
