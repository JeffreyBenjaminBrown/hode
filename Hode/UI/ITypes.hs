{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hode.UI.ITypes where

import           Control.Lens
import           Data.Map (Map)
import qualified Data.List.PointedList as P

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Hode.Hash.HTypes
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Util.Misc
import Hode.Util.PTree


-- | = Tiny types

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

data BufferRow = BufferRow {
    _rsltView :: RsltView
  , _columnProps :: ()
  , _otherProps :: () } deriving (Show, Eq, Ord)

bufferRow_from_rsltView :: RsltView -> BufferRow
bufferRow_from_rsltView rv = BufferRow rv () ()

-- | An `RsltView` is a node in a collecction of search results.
-- Each search returns a flat list of such nodes, but then the user
-- can choose to view the members and hosts of any node, recursively,
-- resulting in a tree.
--
-- | PITFALL: `VTree RsltView` permits invalid state.
-- A `VQuery` should be nowhere but the top of the tree.
-- Subviews of `VQuery`, `VMember`, and `VCenterRole` should be `VExpr`s.
-- The subviews of a `VExpr` should be `VMember`s or `VCenterRole`s.
data RsltView = VQuery       ViewQuery
              | VExpr        ViewExpr
              | VMemberGroup MembersGroup
              | VHostGroup   HostGroup
  deriving (Eq, Ord)

type ViewQuery = String -- ^ What the user asked for

data ViewExpr = ViewExpr {
    _viewResultAddr   :: Addr
  , _viewResultString :: String } deriving (Show, Eq, Ord)

-- | The members of some "center" `Expr`.
data MembersGroup = MembersGroup { _viewMembersCenter :: Addr }
  deriving (Show, Eq, Ord)

-- | The hosts of some "center" `Expr`.
-- If `Expr` `h` hosts the center `Expr` `c`, it could be because
-- (1) `h` is a `Rel`, of which `c` is a member, or
-- (2) `h` is a `Tplt`, in which `c` is a joint
data HostGroup =
  RelHostGroup MemberHosts  -- ^ `Rel`s  that the center is a member of
  | TpltHostGroup JointHosts -- ^ `Tplt`s that the center is a joint in
  deriving (Eq, Ord, Show)

-- | `MemberHosts` is used to group relationships in which the `Expr`at
-- `relHostsCenter` appears. For instance, if the `Expr` at `Addr 3` helps some things,
-- then `MemberHosts 3 (RoleMember 1) ["", "helps", ""]` will
-- be one of the groups of relationships involving the `Expr` at `Addr 3`.
data MemberHosts = MemberHosts {
    _relHostsCenter :: Addr      -- ^ the thing being hosted
  , _relHostsRole   :: Role      -- ^ the role it plays
  , _relHostsTplt   :: Tplt Expr -- ^ the kind of Rel hosting it
  } deriving (Eq, Ord)

data JointHosts = JointHosts { _templatesCenter :: Addr }
  deriving (Eq, Ord)

instance Show RsltView where
  show (VQuery x)       = "VQuery "     ++ show x
  show (VExpr x)        = "VExpr "    ++ show x
  show (VMemberGroup x) = "VMemberGroup "   ++ show x
  show (VHostGroup x)   = "VHostGroup " ++ show x

instance Show MemberHosts where
  show relHosts = let
    tplt = _relHostsTplt relHosts
    noLeft     = error "show MemberHosts: impossible"
    noRslt     = error "show MemberHosts: Rslt irrelevant"
    noMiscount = error "show MemberHosts: This math is good."
    in if _relHostsRole relHosts == RoleTplt
       then "Rels using it (as a Tplt)"
       else let (ar :: Arity) = length tplt - 1
                RoleMember (n :: Int) = _relHostsRole relHosts
                mbrs = either (const noMiscount) id
                       $ replaceNth (Phrase $ "it") n
                       $ replicate ar $ Phrase "_"
            in either (const noLeft) id
               $ eShow noRslt $ ExprRel $ Rel mbrs $ ExprTplt tplt

instance Show JointHosts where
  show _ = "JointHosts in which it is a joint:"

makeLenses ''BufferRow
makePrisms ''RsltView -- prisms
makeLenses ''ViewExpr
makeLenses ''MembersGroup
makeLenses ''MemberHosts


-- | = Huge types.

-- PITFALL: These types must come last in order to derive `Show`.

-- | A `Buffer` displays search results.
-- A user will spend most of their time looking at one of these.
data Buffer = Buffer { _bufferQuery          :: ViewQuery
                     , _bufferRowPorest :: Maybe (Porest BufferRow)
                     } deriving (Eq, Show, Ord)
makeLenses ''Buffer

-- | The entire state of the app.
data St = St {
    _focusRing              :: B.FocusRing BrickName
    -- ^ So far `focusRing` is unused in spirit, but technically used.
  , _searchBuffers          :: Maybe (Porest Buffer)
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

stGetFocusedBuffer :: Getter St (Maybe Buffer)
stGetFocusedBuffer = to go where
  go :: St -> Maybe Buffer
  go st = st ^? searchBuffers . _Just .
    P.focus . getFocusedSubtree . _Just . pTreeLabel

stSetFocusedBuffer :: Setter' St Buffer
stSetFocusedBuffer = sets go where
  go :: (Buffer -> Buffer) -> St -> St
  go f = searchBuffers . _Just . P.focus . setFocusedSubtree .
         pTreeLabel %~ f

stGetFocusedRsltViewTree :: Getter St (Maybe (PTree BufferRow))
stGetFocusedRsltViewTree = to go where
  go :: St -> Maybe (PTree BufferRow)
  go st = st ^? stGetFocusedBuffer . _Just .
    bufferRowPorest . _Just .
    P.focus . getFocusedSubtree . _Just

stSetFocusedRsltViewTree :: Setter' St (PTree BufferRow)
stSetFocusedRsltViewTree = sets go where
  go :: (PTree BufferRow -> PTree BufferRow) -> St -> St
  go f = stSetFocusedBuffer . bufferRowPorest . _Just .
         P.focus . setFocusedSubtree %~ f
