{-# LANGUAGE
LambdaCase,
RankNTypes,
ScopedTypeVariables,
TemplateHaskell
#-}

module Hode.UI.Types.State (
  -- * types and optics
    Buffer(..)
  , bufferExprRowTree      -- ^ PTree ExprRow
  , getBuffer_viewForkType -- ^ Traversal' Buffer ViewForkType

  , St(..)
  , focusRing              -- ^ fetch a B.FocusRing BrickName
  , searchBuffers          -- ^ fetch a Maybe (Porest Buffer)
  , columnHExprs           -- ^ fetch a [HExpr]
  , blockingCycles         -- ^ fetch a Maybe [Cycle]
  , uiError                -- ^ fetch a String
  , reassurance            -- ^ fetch a String
  , commands               -- ^ fetch a B.Editor String BrickName
  , commandHistory         -- ^ fetch a [Command]
  , appRslt                -- ^ fetch a Rslt
  , viewOptions            -- ^ fetch a ViewOptions
  , showingErrorWindow     -- ^ fetch a Bool -- overrides main window
  , showingInMainWindow    -- ^ fetch a MainWindowName
  , showingOptionalWindows -- ^ fetch a Map OptionalWindowName Bool

  -- * misc
  , stGet_focusedBuffer            -- ^ Fold St Buffer
  , stSet_focusedBuffer            -- ^ Setter' St Buffer
  , stGetTopLevelBuffer_byQuery    -- ^ Getter  St (Maybe Buffer)
  , stSetTopLevelBuffer_byQuery    -- ^ Setter' St Buffer
  , stGetFocused_ViewExprNode_Tree -- ^ Fold St (PTree ExprRow)
  , stSetFocused_ViewExprNode_Tree -- ^ Setter' St (PTree ExprRow)
  , resultWindow_focusAddr  -- ^ St -> Either String Addr
  , stFocusPeers            -- ^ Fold St (Porest ExprRow)
  , stFocusGroupOrder -- ^ Fold St (BinOrientation, TpltAddr)
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.Hash.Types
import Hode.PTree.Initial
import Hode.Rslt.Binary (BinOrientation)
import Hode.Rslt.Types
import Hode.UI.Types.Names
import Hode.UI.Types.Views


-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer
  { _bufferExprRowTree :: PTree ExprRow
  } deriving (Eq, Show, Ord)
makeLenses ''Buffer

getBuffer_viewForkType :: Traversal' Buffer ViewForkType
getBuffer_viewForkType =
  bufferExprRowTree . pTreeLabel .
  viewExprNode . _VenFork . viewForkType

-- | The entire state of the app.
data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ Technically used, but unused in spirit.
  , _searchBuffers          :: Maybe (Porest Buffer)
  , _columnHExprs           :: [HExpr]
  , _commandHistory         :: [Command]
  , _blockingCycles         :: Maybe [Cycle]

  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String BrickName

  , _appRslt                :: Rslt
  , _viewOptions            :: ViewOptions
  , _showingErrorWindow     :: Bool -- ^ overrides main window
  , _showingInMainWindow    :: MainWindowName
  , _showingOptionalWindows :: Map OptionalWindowName Bool
  }
makeLenses ''St

stGet_focusedBuffer :: Fold St Buffer
stGet_focusedBuffer =
  searchBuffers . _Just . P.focus .
  getFocusedSubtree . _Just . pTreeLabel

stSet_focusedBuffer :: Setter' St Buffer
stSet_focusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = searchBuffers . _Just . P.focus .
         setFocusedSubtree . pTreeLabel %~ f

-- | PITFALL: Assumes the target buffer is top-level and unique.
-- TODO : This would be more natural as a Fold that returns a Buffer
-- (i.e. without using Maybe).
stGetTopLevelBuffer_byQuery :: ViewQuery -> Getter St (Maybe Buffer)
stGetTopLevelBuffer_byQuery vq = to go where
  go :: St -> Maybe Buffer
  go st = case  _searchBuffers st of
    Nothing -> Nothing
    Just p ->
      ( \case [] -> Nothing;   a:_ -> Just a )
      . filter (   (== Just (VFQuery vq) )
                 . (^? getBuffer_viewForkType ) )
      . toList . fmap _pTreeLabel
      $ p

-- | PITFALL: Assumes the Cycle Buffer is top-level and unique.
stSetTopLevelBuffer_byQuery :: ViewQuery -> Setter' St Buffer
stSetTopLevelBuffer_byQuery vq = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f st = let
    g :: Buffer -> Buffer
    g b = if b ^? getBuffer_viewForkType
             == Just (VFQuery vq)
          then f b else b
    in st & searchBuffers . _Just
          %~ fmap (pTreeLabel %~ g)

stGetFocused_ViewExprNode_Tree ::
  Fold St (PTree ExprRow)
stGetFocused_ViewExprNode_Tree =
  stGet_focusedBuffer . bufferExprRowTree
  . getFocusedSubtree . _Just

stSetFocused_ViewExprNode_Tree ::
  Setter' St (PTree ExprRow)
stSetFocused_ViewExprNode_Tree = sets go where
  go :: (PTree ExprRow -> PTree ExprRow) -> St -> St
  go f = stSet_focusedBuffer .
         bufferExprRowTree .
         setFocusedSubtree %~ f

resultWindow_focusAddr :: St -> Either String Addr
resultWindow_focusAddr st =
  maybe (error "Focused ViewExprNode not found.") Right
    (st ^? stGetFocused_ViewExprNode_Tree)
    >>= exprTree_focusAddr

stFocusPeers :: Fold St (Porest ExprRow)
stFocusPeers =
  stGet_focusedBuffer . bufferExprRowTree . getPeersOfFocusedSubtree

-- | The focused `ExprRow`s parent's children might be sorted.
-- If so, this returns the order on them.
stFocusGroupOrder :: Fold St (BinOrientation, TpltAddr)
stFocusGroupOrder =
  stGet_focusedBuffer . bufferExprRowTree
  . getParentOfFocusedSubtree . _Just
  . pTreeLabel . otherProps . childSort . _Just
