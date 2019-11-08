{-# LANGUAGE
DeriveFunctor, DeriveFoldable, DeriveTraversable,
RankNTypes,
ScopedTypeVariables,
TemplateHaskell,
ViewPatterns,
TypeFamilies #-}

module Hode.UI.ITypes where

import           Control.Lens
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.Brick
import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Qseq.QTypes
import Hode.Rslt.Binary
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Rslt.ShowAttr
import Hode.Util.Misc
import Hode.Util.PTree


-- | = Tiny types: names for windows, commands, folders

-- | PITFALL: Some window names never reach Brick. Ones that do reach
-- Brick must be unique across windows in any drawn image. (Not every
-- window Brick draws needs a name. Editors and viewports in particular do.)
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName MainWindowName
  deriving (Ord, Show, Eq)
data OptionalWindowName = Commands
                        | Reassurance
  deriving (Ord, Show, Eq)
data MainWindowName = CommandHistory
                    | Results
                    | SearchBuffers
  deriving (Ord, Show, Eq)

data Command =
    CommandInsert       Expr
  | CommandReplace Addr Expr
  | CommandDelete  Addr
  | CommandFind     String HExpr
  | CommandFindSort String HExpr BinOrientation TpltAddr
  | CommandLoad Folder
  | CommandSave Folder
  deriving (Show, Eq, Ord)

type Folder = String


-- | = Views

type ColumnProps = Map HExpr Int

data OtherProps = OtherProps {
  _folded :: Bool -- ^ whether the ViewExprNode's children are hidden
  } deriving (Show, Eq, Ord)

data BufferRow = BufferRow {
    _viewExprNode :: ViewExprNode
  , _columnProps  :: ColumnProps
  , _otherProps   :: OtherProps
  } deriving (Show, Eq, Ord)

bufferRow_from_viewExprNode :: ViewExprNode -> BufferRow
bufferRow_from_viewExprNode n =
  BufferRow n mempty $ OtherProps False

-- | = A `ViewExprNode` is a node in a tree of descendents of search results.
-- Each search returns a flat list of `ViewExprNode`s.
-- The user can then choose to view members and hosts of any node,
-- recursively, thus building a "view tree".
--
-- A `VMemberFork`  or `VHostFork` announces the relationship
-- between its parent in the view tree and its children.
--
-- PITFALL: `VTree ViewExprNode` permits invalid state.
-- A `VQuery` should be nowhere but the top of the tree.
-- Subviews of `VQuery`, `VMember`, and `VCenterRole` should be `VExpr`s.
-- The subviews of a `VExpr` should be `VMemberFork`s or `VHostFork`s.

data ViewExprNode =
    VQuery      ViewQuery  -- ^ The top of every view tree is this.
  | VExpr       ViewExpr   -- ^ Corresponds to some `Expr`.
  | VMemberFork MemberFork -- ^ Announces the relationship between its
                           -- parent in the view tree and its children.
  | VHostFork   HostFork   -- ^ Announces the relationship between its
                           -- parent in the view tree and its children.
  deriving (Eq, Ord, Show)

type ViewQuery = String -- ^ What the user asked for.

data ViewExpr = ViewExpr {
    _viewExpr_Addr   :: Addr
  , _viewExpr_String :: AttrString }
  deriving (Show, Eq, Ord)

-- | Announces the members of some "center" `Expr`.
data MemberFork = MemberFork {
  _membersForkCenter :: Addr }
  deriving (Show, Eq, Ord)

-- | Announces some `Expr`s in which the "center" `Expr`
-- is involved.
data HostFork =
    RelHostFork  RelHosts   -- ^ `Rel`s  that the center is a member of
  | TpltHostFork TpltHosts  -- ^ `Tplt`s that the center is a joint in
  deriving (Eq, Ord, Show)

data RelHosts = RelHosts {
    _memberHostsCenter :: Addr      -- ^ the `RelHosts`
      -- describes some `Rel`s involving the `Expr` here
  , _memberHostsRole   :: Role      -- ^ that `Expr` plays
      -- this `Role` in each of those `Rel`s
  , _memberHostsTplt   :: Tplt Expr -- ^ and each of those
     -- `Rel`s has this `Tplt`
  } deriving (Eq, Ord)

-- | `TpltHosts` is used to group `Tplt`s to which the
-- `Expr` at `jointHostsCenter` belongs.
data TpltHosts = TpltHosts {
  _jointHostsCenter :: Addr }
  deriving (Eq, Ord)

-- | Shows the label of the group, not its members.
instance Show RelHosts where
  -- PITFALL: Egregious duplication; see `ShowAttr` instance.
  show (_memberHostsRole -> RoleInRel' RoleTplt) =
    "Rels using it as a Tplt"
  show relHosts = let
    tplt :: Tplt Expr = _memberHostsTplt relHosts
    noLeft     = error "show RelHosts: impossible"
    noRslt     = error "show RelHosts: Rslt irrelevant"
    noMiscount = error "show RelHosts: Did I miscount?"
    RoleInRel' (RoleMember (n :: Int)) =
      _memberHostsRole relHosts
    mbrs = either (const noMiscount) id
           $ replaceNth (Phrase $ "it") n
           $ replicate (arity tplt) $ Phrase "_"
    in either (const noLeft) id $
       eParenShow 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

instance ShowAttr RelHosts where
  -- PITFALL: Egregious duplication; see `Show` instance.
  showAttr (_memberHostsRole -> RoleInRel' RoleTplt) =
    [("Rels using it as a Tplt",textColor)]
  showAttr relHosts = let
    tplt :: Tplt Expr = _memberHostsTplt relHosts
    noLeft     = error "show RelHosts: impossible"
    noRslt     = error "show RelHosts: Rslt irrelevant"
    noMiscount = error "show RelHosts: Did I miscount?"
    RoleInRel' (RoleMember (n :: Int)) =
      _memberHostsRole relHosts
    mbrs = either (const noMiscount) id
           $ replaceNth (Phrase $ "it") n
           $ replicate (arity tplt) $ Phrase "_"
    in either (const noLeft) id $
       eParenShowAttr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

-- | Shows the label of the group, not its members.
instance Show TpltHosts where
  show _ = "Tplts using it as a joint"

makeLenses ''BufferRow
makeLenses ''OtherProps
makePrisms ''ViewExprNode -- prisms
makeLenses ''ViewExpr
makeLenses ''MemberFork
makeLenses ''RelHosts


-- | Whereas `show` shows everything about the `ViewExprNode`,
-- `showBrief` hides things the UI already makes clear.
instance ShowBrief ViewExprNode where
  showBrief (VQuery vq) = vq
  showBrief (VExpr x) =
    show (x ^. viewExpr_Addr) ++ ": "
    ++ show (x ^. viewExpr_String)
  showBrief (VMemberFork _) = "its members"
  showBrief (VHostFork (RelHostFork  x)) = show x
  showBrief (VHostFork (TpltHostFork x)) = show x

instance ShowAttr ViewExprNode where
  showAttr (VExpr ve) =
    [(show $ _viewExpr_Addr ve, addrColor)]
    ++ _viewExpr_String ve
  showAttr (VHostFork (RelHostFork r)) =
    showAttr r
  showAttr x =
    [(showBrief x, textColor)]


-- | = Huge types.

-- PITFALL: These types must come last in order to derive `Show`.

-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer
  { _bufferQuery     :: ViewQuery
  , _bufferRowPorest :: Maybe (Porest BufferRow)
  } deriving (Eq, Show, Ord)
makeLenses ''Buffer

-- | The entire state of the app.
data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far unused in spirit, but technically used.
  , _searchBuffers          :: Maybe (Porest Buffer)
  , _columnHExprs           :: [HExpr]
  , _uiError                :: String
  , _reassurance            :: String
  , _commands               :: B.Editor String BrickName
  , _commandHistory         :: [Command]
  , _appRslt                :: Rslt
  , _showingErrorWindow     :: Bool -- ^ overrides main window
  , _showingInMainWindow    :: MainWindowName
  , _showingOptionalWindows :: Map OptionalWindowName Bool
  }
makeLenses ''St

bufferRow_from_viewExprNode'
  :: St -> ViewExprNode -> Either String BufferRow
bufferRow_from_viewExprNode' st n@(VExpr (ViewExpr a _)) =
  prefixLeft "bufferRow_from_viewExprNode': " $ do
  let r = st ^. appRslt
      hs = st ^. columnHExprs
      sub :: Map Var Addr =
        M.singleton VarRowNode a
  matches :: Map HExpr (Set Addr) <-
    let f h = (h, hExprToAddrs r sub h)
    in ifLefts_map $ M.fromList $ map f hs
  let matchCounts :: Map HExpr Int =
        M.map S.size matches
  Right $ BufferRow n matchCounts $ OtherProps False
bufferRow_from_viewExprNode' _ n =
  Right $ bufferRow_from_viewExprNode n

stGetFocused_Buffer :: Getter St (Maybe Buffer)
stGetFocused_Buffer = to go where
  go :: St -> Maybe Buffer
  go st = st ^? searchBuffers . _Just .
    P.focus . getFocusedSubtree . _Just . pTreeLabel

stSetFocusedBuffer :: Setter' St Buffer
stSetFocusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = searchBuffers . _Just . P.focus . setFocusedSubtree .
         pTreeLabel %~ f

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
  go f = stSetFocusedBuffer . bufferRowPorest . _Just .
         P.focus . setFocusedSubtree %~ f
