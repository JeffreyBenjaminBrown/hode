{-# LANGUAGE
ScopedTypeVariables,
MultiParamTypeClasses,
TemplateHaskell,
ViewPatterns
#-}

module Hode.UI.Types.Views where

import Control.Lens
import Data.Set (Set)
import Data.Map (Map)

import Hode.Brick
import Hode.Hash.HTypes
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Rslt.ShowColor
import Hode.Util.Misc


data ViewOptions = ViewOptions
  { _viewOpt_ShowAddresses :: Bool
    -- ^ Whether to show its address next to each `Expr`.
  , _viewOpt_ShowAsAddresses :: Bool
    -- ^ Whether to reduce redundancy
    -- by sometimes replacing an `Expr` with its address.
  , _viewOpt_WrapLength :: Int }
    -- ^ How many characters to show before wrapping
    -- around to the left side of the screen.
    -- (If I was better with Brick, I might not need this.)
makeLenses ''ViewOptions

type ColumnProps = Map HExpr Int

data OtherProps = OtherProps {
  _folded :: Bool -- ^ whether the ViewExprNode's children are hidden
  } deriving (Show, Eq, Ord)

-- | What the user searched for.
data ViewQuery = QueryView String | CycleView
  deriving (Eq, Ord, Show)
-- PITFALL: These `Prism`s have to be defined *right here*.
makePrisms ''ViewQuery

data ExprRow = ExprRow {
    _viewExprNode :: ViewExprNode
  , _columnProps  :: ColumnProps
  , _otherProps   :: OtherProps
  } deriving (Show, Eq, Ord)


-- | = A `ViewExprNode` is a node in a tree of descendents of search results.
-- Each search returns a flat list of `ViewExprNode`s.
-- The user can then choose to view members and hosts of any node,
-- recursively, thus building a "view tree".
--
-- A `VMemberFork`  or `VHostFork` announces the relationship
-- between its parent in the view tree and its children.
--
-- PITFALL: `PTree ViewExprNode` permits invalid state.
-- A `VQuery` should be nowhere but the top of the tree.
-- Subviews of `VQuery`, `VMember`, and `VCenterRole` should be `VExpr`s.
-- The subviews of a `VExpr` should be `VMemberFork`s or `VHostFork`s.

data ViewExprNode =
    VQuery      ViewQuery -- ^ The top of every view tree is this.
  | VExpr       ViewExpr -- ^ Corresponds to some `Expr`.
  | VMemberFork -- ^ Announces the relationship between its
                -- parent in the view tree and its children.
  | VHostFork   HostFork -- ^ Announces the relationship between its
                         -- parent in the view tree and its children.
  | VSearchFork -- ^ Announces the relationship between its view-parent
                -- and the results when that is evaluated as a search.
  deriving (Eq, Ord, Show)

data ViewExpr = ViewExpr {
    _viewExpr_Addr        :: Addr
  , _viewExpr_showAsAddrs :: Set Addr
    -- ^ If the relevant `viewOpt_ShowAsAddresses` is True,
    -- any sub-`Expr` at any of these `Addr`s will be shown
    -- not as text, but as the `Addr` in question.
    -- Used to eliminate redundancy in views.
  , _viewExpr_String      :: ColorString }
  deriving (Show, Eq, Ord)

-- | Announces some `Expr`s in which the "center" `Expr`
-- is involved.
data HostFork =
    RelHostFork  RelHosts   -- ^ `Rel`s  that the center is a member of
  | TpltHostFork TpltHosts  -- ^ `Tplt`s that the center is a separator in
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
-- `Expr` at `separatorHostsCenter` belongs.
data TpltHosts = TpltHosts {
  _separatorHostsCenter :: Addr }
  deriving (Eq, Ord)

-- TODO ? this class, and the two *Center fields above, are unused.
-- See that with `grep -i center -r UI --color`.
class HasCenterAddr a where
  centerAddr :: a -> Addr

instance HasCenterAddr TpltHosts where
  centerAddr = _separatorHostsCenter

instance HasCenterAddr RelHosts where
  centerAddr = _memberHostsCenter

instance HasCenterAddr HostFork where
  centerAddr (RelHostFork f) = centerAddr f
  centerAddr (TpltHostFork f) = centerAddr f


-- | = Functions (inc. TH-generated lenses)

-- | Shows the label of the group, not its members.
instance Show RelHosts where
  -- PITFALL: Egregious duplication; see `ShowColor` instance.
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
       eParenShowExpr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

instance ShowColor ViewOptions RelHosts where
  -- PITFALL: Egregious duplication; see `Show` instance.
  showColor _ (_memberHostsRole -> RoleInRel' RoleTplt) =
    [("Rels using it as a Tplt",TextColor)]
  showColor _ relHosts = let
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
       eParenShowColorExpr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

-- | Shows the label of the group, not its members.
instance Show TpltHosts where
  show _ = "Tplts using it as a separator"

-- PITFALL: These `Lens`es have to be defined *right here*,
-- after the `Show` instances that `ViewExprNode` depends on,
-- and before the `Show` instances for `ViewExprNode` itself.
makeLenses ''ExprRow
makeLenses ''OtherProps
makePrisms ''ViewExprNode -- prisms
makeLenses ''ViewExpr
makeLenses ''RelHosts

-- | Whereas `show` shows everything about the `ViewExprNode`,
-- `showBrief` hides things the UI already makes clear.
instance ShowBrief ViewExprNode where
  showBrief (VQuery (QueryView vq)) = vq
  showBrief (VQuery CycleView) = "The following cycle exists,"
    ++ " in what should be transitive relationships."
  showBrief (VExpr x) =
    show (x ^. viewExpr_Addr) ++ ": "
    ++ unColorString (x ^. viewExpr_String)
  showBrief VMemberFork = "its members"
  showBrief (VHostFork (RelHostFork  x)) = show x
  showBrief (VHostFork (TpltHostFork x)) = show x
  showBrief VSearchFork = "its search results"

instance ShowColor ViewOptions ViewExprNode where
  showColor vo (VExpr ve) =
    ( if _viewOpt_ShowAddresses vo
      then [(show (_viewExpr_Addr ve) ++ " ", AddrColor)]
      else [] )
    ++ _viewExpr_String ve
  showColor vo (VHostFork (RelHostFork r)) =
    showColor vo r
  showColor _ x =
    [(showBrief x, TextColor)]
