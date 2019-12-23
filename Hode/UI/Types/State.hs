{-# LANGUAGE
ScopedTypeVariables,
TemplateHaskell
#-}

module Hode.UI.Types.State where

import           Control.Lens
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.UI.Types.Names
import Hode.UI.Types.Views

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Qseq.QTypes
import Hode.Rslt.RTypes
import Hode.Util.Misc
import Hode.PTree.Initial


-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer
  { _bufferQuery     :: ViewQuery
  , _bufferRowPorest :: Maybe (Porest BufferRow)
  , _bufferCycles    :: [[Addr]]
  } deriving (Eq, Show, Ord)
makeLenses ''Buffer

-- | The entire state of the app.
data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far unused in spirit, but technically used.
  , _searchBuffers          :: Maybe (Porest Buffer)
  , _columnHExprs           :: [HExpr]
  , _cycleBreaker           :: Porest BufferRow
    -- ^ like a Buffer, but without a title or list of cycles
  , _blockedByCycles        :: Bool
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

bufferRow_from_viewExprNode'
  :: St -> ViewExprNode -> Either String BufferRow
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
  Right $ BufferRow { _viewExprNode = n
                    , _columnProps = matchCounts
                    , _otherProps = OtherProps False }
bufferRow_from_viewExprNode' _ n =
  Right $ bufferRow_from_viewExprNode n

stGetFocused_Buffer :: Getter St (Maybe Buffer)
stGetFocused_Buffer = to go where
  go :: St -> Maybe Buffer
  go st = st ^? searchBuffers . _Just . P.focus .
          getFocusedSubtree . _Just . pTreeLabel

stSetFocusedBuffer :: Setter' St Buffer
stSetFocusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = searchBuffers . _Just . P.focus .
         setFocusedSubtree . pTreeLabel %~ f

stGetFocused_ViewExprNode_Tree ::
  Getter St (Maybe (PTree BufferRow))
stGetFocused_ViewExprNode_Tree = to go where
  go :: St -> Maybe (PTree BufferRow)
  go st = st ^? stGetFocused_Buffer . _Just .
          bufferRowPorest . _Just .
          P.focus . getFocusedSubtree . _Just

stSetFocused_ViewExprNode_Tree ::
  Setter' St (PTree BufferRow)
stSetFocused_ViewExprNode_Tree = sets go where
  go :: (PTree BufferRow -> PTree BufferRow) -> St -> St
  go f = stSetFocusedBuffer .
         bufferRowPorest . _Just .
         P.focus . setFocusedSubtree %~ f

focusAddr :: St -> Either String Addr
focusAddr st = do
  foc :: PTree BufferRow <-
    maybe (error "Focused ViewExprNode not found.") Right $
    st ^? stGetFocused_ViewExprNode_Tree . _Just
  case foc ^. pTreeLabel . viewExprNode of
    VExpr rv -> Right $ rv ^. viewExpr_Addr
    _        -> Left $ "Can only be called from a VExpr."
