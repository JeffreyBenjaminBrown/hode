{-# LANGUAGE
ScopedTypeVariables,
TemplateHaskell
#-}

module Hode.UI.Types.State where

import           Control.Lens
import           Data.Map (Map)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.UI.Types.Names
import Hode.UI.Types.Views

import Hode.Hash.HTypes
import Hode.Rslt.RTypes
import Hode.PTree.Initial


-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer
  { _bufferQuery     :: ViewQuery
  , _bufferRowPorest :: Maybe (Porest ExprRow)
  } deriving (Eq, Show, Ord)
makeLenses ''Buffer

-- | The entire state of the app.
data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ Technically used, but unused in spirit.
  , _searchBuffers          :: Maybe (Porest Buffer)
  , _columnHExprs           :: [HExpr]
  , _cycleBuffer            :: Porest ExprRow
    -- ^ Like a search Buffer, but with no title.
  , _blockingCycles         :: Maybe [Cycle]
  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String BrickName
  , _commandHistory         :: [Command]
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

stSet_focusedBuffer :: Setter' St Buffer
stSet_focusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = searchBuffers . _Just . P.focus .
         setFocusedSubtree . pTreeLabel %~ f

stGetFocused_ViewExprNode_Tree ::
  Getter St (Maybe (PTree ExprRow))
stGetFocused_ViewExprNode_Tree = to go where
  go :: St -> Maybe (PTree ExprRow)
  go st = st ^? stGet_focusedBuffer . _Just .
          bufferRowPorest . _Just .
          P.focus . getFocusedSubtree . _Just

stSetFocused_ViewExprNode_Tree ::
  Setter' St (PTree ExprRow)
stSetFocused_ViewExprNode_Tree = sets go where
  go :: (PTree ExprRow -> PTree ExprRow) -> St -> St
  go f = stSet_focusedBuffer .
         bufferRowPorest . _Just .
         P.focus . setFocusedSubtree %~ f

focusAddr :: St -> Either String Addr
focusAddr st = do
  foc :: PTree ExprRow <-
    maybe (error "Focused ViewExprNode not found.") Right $
    st ^? stGetFocused_ViewExprNode_Tree . _Just
  case foc ^. pTreeLabel . viewExprNode of
    VExpr rv -> Right $ rv ^. viewExpr_Addr
    _        -> Left $ "Can only be called from a VExpr."

