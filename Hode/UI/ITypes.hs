{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hode.UI.ITypes where

import           Control.Lens
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.Brick (AttrString)
import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Qseq.QTypes
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Util.Misc
import Hode.Util.PTree


-- | = Tiny types: names for windows, commands, folders

-- | PITFALL: Some window names never reach Brick. Ones that do reach
-- Brick must be unique across windows in any drawn image. (Not every
-- window Brick draws needs a name. Editors and viewports in particular do.)
data BrickName = BrickOptionalName OptionalWindowName
               | BrickMainName MainWindowName deriving (Ord, Show, Eq)
data OptionalWindowName = Commands
                        | Reassurance deriving (Ord, Show, Eq)
data MainWindowName = CommandHistory
                    | Results
                    | SearchBuffers deriving (Ord, Show, Eq)

data Command = CommandInsert       Expr
             | CommandReplace Addr Expr
             | CommandDelete  Addr
             | CommandFind String HExpr
             | CommandLoad Folder
             | CommandSave Folder deriving (Show, Eq, Ord)

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

type ViewQuery = String -- ^ What the user asked for.

data ViewExpr = ViewExpr {
    _viewExpr_Addr   :: Addr
  , _viewExpr_String :: AttrString } deriving (Show, Eq, Ord)

-- | The members of some "center" `Expr`.
data MemberFork = MemberFork { _membersForkCenter :: Addr }
  deriving (Show, Eq, Ord)

-- | The hosts of some "center" `Expr`.
-- If `Expr` `h` hosts the center `Expr` `c`, it could be because
-- (1) `h` is a `Rel`, of which `c` is a member, or
-- (2) `h` is a `Tplt`, in which `c` is a joint
data HostFork =
    RelHostFork RoleHosts    -- ^ `Rel`s  that the center is a member of
  | TpltHostFork JointHosts  -- ^ `Tplt`s that the center is a joint in
  deriving (Eq, Ord, Show)

-- | `RoleHosts` is used to group relationships to which the `Expr` at
-- `memberHostsCenter` belongs.
-- For instance, if the `Expr` at `Addr 3` helps some things,
-- then `RoleHosts 3 (RoleMember 1) ["", "helps", ""]` will
-- be one of the groups of relationships involving the `Expr` at `Addr 3`.
data RoleHosts = RoleHosts {
    _memberHostsCenter :: Addr      -- ^ what plays the `Role`
  , _memberHostsRole   :: Role      -- ^ the `Role` it plays
  , _memberHostsTplt   :: Tplt Expr -- ^ the kind of `Rel` hosting it
  } deriving (Eq, Ord)

-- | `JointHosts` is used to group `Tplt`s to which the `Expr` at
-- `jointHostsCenter` belongs.
data JointHosts = JointHosts { _jointHostsCenter :: Addr }
  deriving (Eq, Ord)

-- | Shows the label of the group, not its members.
instance Show RoleHosts where
  show relHosts = let
    tplt :: Tplt Expr = _memberHostsTplt relHosts
    noLeft     = error "show RoleHosts: impossible"
    noRslt     = error "show RoleHosts: Rslt irrelevant"
    noMiscount = error "show RoleHosts: This math is good."
    in if _memberHostsRole relHosts == RoleInRel' RoleTplt
       then " Rels using it as a Tplt"
       else let RoleInRel' (RoleMember (n :: Int)) =
                  _memberHostsRole relHosts
                mbrs = either (const noMiscount) id
                       $ replaceNth (Phrase $ "it") n
                       $ replicate (arity tplt) $ Phrase "_"
            in (" " ++) $
               either (const noLeft) id $
               eParenShow 3 noRslt $ ExprRel $ Rel mbrs $ ExprTplt tplt

-- | Shows the label of the group, not its members.
instance Show JointHosts where
  show _ = "JointHosts in which it is a joint:"

data ViewExprNode =
    VQuery      ViewQuery  -- ^ The top of every view tree is this.
  | VExpr       ViewExpr   -- ^ Corresponds to some `Expr`.
  | VMemberFork MemberFork -- ^ Announces the relationship between its
                           -- parent in the view tree and its children.
  | VHostFork   HostFork   -- ^ Announces the relationship between its
                           -- parent in the view tree and its children.
  deriving (Eq, Ord)

instance Show ViewExprNode where
   show (VQuery x)      = "VQuery "       ++ show x
   show (VExpr x)       = "VExpr "        ++ show x
   show (VMemberFork x) = "VMemberFork " ++ show x
   show (VHostFork x)   = "VHostFork "   ++ show x

makeLenses ''BufferRow
makeLenses ''OtherProps
makePrisms ''ViewExprNode -- prisms
makeLenses ''ViewExpr
makeLenses ''MemberFork
makeLenses ''RoleHosts


-- | = Huge types.

-- PITFALL: These types must come last in order to derive `Show`.

-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer { _bufferQuery     :: ViewQuery
                     , _bufferRowPorest :: Maybe (Porest BufferRow)
                     } deriving (Eq, Show, Ord)
makeLenses ''Buffer

-- | The entire state of the app.
data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far `focusRing` is unused in spirit, but technically used.
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
      sub :: Map Var Addr = M.singleton VarRowNode a
  matches :: Map HExpr (Set Addr) <-
    let f h = (h, hExprToAddrs r sub h)
    in ifLefts_map $ M.fromList $ map f hs
  let matchCounts :: Map HExpr Int =
        M.map S.size matches
  Right $ BufferRow n matchCounts $ OtherProps False
bufferRow_from_viewExprNode' _ n =
  Right $ BufferRow n mempty $ OtherProps False

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

stGetFocused_ViewExprNode_Tree :: Getter St (Maybe (PTree BufferRow))
stGetFocused_ViewExprNode_Tree = to go where
  go :: St -> Maybe (PTree BufferRow)
  go st = st ^? stGetFocused_Buffer . _Just .
    bufferRowPorest . _Just .
    P.focus . getFocusedSubtree . _Just

stSetFocused_ViewExprNode_Tree :: Setter' St (PTree BufferRow)
stSetFocused_ViewExprNode_Tree = sets go where
  go :: (PTree BufferRow -> PTree BufferRow) -> St -> St
  go f = stSetFocusedBuffer . bufferRowPorest . _Just .
         P.focus . setFocusedSubtree %~ f
