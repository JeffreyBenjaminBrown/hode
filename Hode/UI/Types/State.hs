{-# LANGUAGE
LambdaCase,
ScopedTypeVariables,
TemplateHaskell
#-}

module Hode.UI.Types.State (
  -- * types and optics
    Buffer(..)
  , bufferExprRowTree     -- ^ PTree ExprRow
  , getBuffer_viewForkType -- ^ NOT this type, but close:
                           -- Prism' Buffer ViewForkType

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
  , stGet_focusedBuffer            -- ^ Getter  St (Maybe Buffer)
  , stGet_cycleBuffer              -- ^ Getter  St (Maybe Buffer)
  , stSet_focusedBuffer            -- ^ Setter' St Buffer
  , stSet_cycleBuffer              -- ^ Setter' St Buffer
  , stGetFocused_ViewExprNode_Tree -- ^ Getter  St (Maybe (PTree ExprRow))
  , stSetFocused_ViewExprNode_Tree -- ^ Setter' St (PTree ExprRow)
  , resultWindow_focusAddr -- ^ St -> Either String Addr
  , stFocusPeers           -- ^ St -> Maybe (Porest ExprRow)
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.Hash.Types
import Hode.PTree.Initial
import Hode.Rslt.Types
import Hode.UI.Types.Names
import Hode.UI.Types.Views


-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer
  { _bufferExprRowTree :: PTree ExprRow
  } deriving (Eq, Show, Ord)
makeLenses ''Buffer

-- | I want to say this has type `Prism' Buffer ViewForkType`,
-- but that appears not to be true.
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

stGet_focusedBuffer :: Getter St (Maybe Buffer)
stGet_focusedBuffer = to go where
  go :: St -> Maybe Buffer
  go st = st ^? searchBuffers . _Just . P.focus .
          getFocusedSubtree . _Just . pTreeLabel

-- | PITFALL: Assumes the Cycle Buffer is top-level and unique.
stGet_cycleBuffer :: Getter St (Maybe Buffer)
stGet_cycleBuffer = to go where
  go :: St -> Maybe Buffer
  go st = case  _searchBuffers st of
    Nothing -> Nothing
    Just p ->
      ( \case [] -> Nothing;   a:_ -> Just a )
      . filter (   (== Just (VFQuery CycleView) )
                 . (^? getBuffer_viewForkType ) )
      . toList . fmap _pTreeLabel
      $ p

stSet_focusedBuffer :: Setter' St Buffer
stSet_focusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = searchBuffers . _Just . P.focus .
         setFocusedSubtree . pTreeLabel %~ f

-- | PITFALL: Assumes the Cycle Buffer is top-level and unique.
stSet_cycleBuffer :: Setter' St Buffer
stSet_cycleBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f st = let
    g :: Buffer -> Buffer
    g b = if b ^? getBuffer_viewForkType
             == Just (VFQuery CycleView)
          then f b else b
    in st & searchBuffers . _Just
          %~ fmap (pTreeLabel %~ g)

stGetFocused_ViewExprNode_Tree ::
  Getter St (Maybe (PTree ExprRow))
stGetFocused_ViewExprNode_Tree = to go where
  go :: St -> Maybe (PTree ExprRow)
  go st = st ^? stGet_focusedBuffer . _Just .
          bufferExprRowTree .
          getFocusedSubtree . _Just

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
    (st ^? stGetFocused_ViewExprNode_Tree . _Just)
    >>= exprTree_focusAddr

stFocusPeers :: St -> Either String (Porest ExprRow)
stFocusPeers st =
  case st ^? ( stGet_focusedBuffer . _Just . bufferExprRowTree
               . getParentOfFocusedSubtree . _Just . pMTrees . _Just )
  of Just x  -> Right x
     Nothing -> Left $ "Sort failed. Probably because the focused node is the root of the view, so it has no peers to sort."
