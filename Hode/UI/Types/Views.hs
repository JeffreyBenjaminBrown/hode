-- | PITFALL: Most of this builds toward the `ViewExprNode` type.
-- Read the comment above that type first.

{-# LANGUAGE
ScopedTypeVariables,
LambdaCase,
MultiParamTypeClasses,
TemplateHaskell,
ViewPatterns
#-}

module Hode.UI.Types.Views (
  -- * types and optics
    ViewOptions(..), viewOpt_ShowAddresses, viewOpt_ShowAsAddresses
  , viewOpt_WrapLength
  , NumColumnProps
  , OtherProps(..), folded
  , ViewQuery(..), _QueryView, _CycleView
  , ViewExpr(..), viewExpr_Addr, viewExpr_showAsAddrs, viewExpr_String
  , RelHostGroup(..), memberHostsRole, memberHostsTplt
  , ViewForkType(..), _VFQuery, _VFMembers, _VFTpltHosts
    , _VFRelHosts, _VFSearch
  , ViewFork(..), viewForkCenter, viewForkSortTplt, viewForkType
  , ViewExprNode(..), _VenExpr, _VenFork
  , ExprRow(..), viewExprNode, numColumnProps, otherProps

  -- * misc
  , exprTree_focusAddr -- ^ PTree ExprRow -> Either String Addr
  ) where

import Control.Lens hiding (folded)
import Data.Set (Set)
import Data.Map (Map)

import Hode.Brick
import Hode.Hash.HTypes
import Hode.PTree.Initial
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
  deriving (Show, Eq, Ord)
makeLenses ''ViewOptions

-- | Each `Expr` in the graph is shown on a separate row (or rows).
-- At their left appear a set of columns of numbers.
-- Each column corresponds to a particular `HExpr`.
type NumColumnProps = Map HExpr Int

data OtherProps = OtherProps {
  _folded :: Bool -- ^ whether a `ViewExprNode`'s children are hidden
  } deriving (Show, Eq, Ord)
makeLenses ''OtherProps

-- | Should be at the top of every PTree of ViewExprs.
data ViewQuery
  = QueryView String -- ^ the String is what was searched for
  | CycleView -- ^ When Hode finds a cycle, it makes one of these.
  deriving (Eq, Ord, Show)
makePrisms ''ViewQuery

-- | In order to view a `Expr`, all we need is a `ColorString`.
-- However, we might need to recompute that `ColorString`,
-- which is why we retain the other information.
data ViewExpr = ViewExpr {
    _viewExpr_Addr        :: Addr
  , _viewExpr_showAsAddrs :: Set Addr
    -- ^ If the relevant `viewOpt_ShowAsAddresses` is True,
    -- any sub-`Expr` at any of these `Addr`s will be shown
    -- not as text, but as the `Addr` in question.
    -- Used to eliminate redundancy in views.
  , _viewExpr_String      :: ColorString }
  deriving (Show, Eq, Ord)
makeLenses ''ViewExpr

-- | Example: If `x :: Expr` is in some relationships of the form
-- `x #is _`, that group of relationships will have a `Role` of
-- "member 1" and a `Tplt` of "_ is _".
data RelHostGroup = RelHostGroup { _memberHostsRole :: Role
                                 , _memberHostsTplt :: Tplt Expr }
                  deriving (Eq, Ord)
makeLenses ''RelHostGroup

-- | Shows the label of the group, not its members.
instance Show RelHostGroup where
  -- PITFALL: Egregious duplication; see `ShowColor` instance.
  show rhg = let
    tplt :: Tplt Expr = _memberHostsTplt rhg
    noLeft     = error "show RelHostGroup: impossible"
    noRslt     = error "show RelHostGroup: Rslt irrelevant"
    noMiscount = error "show RelHostGroup: Did I miscount?"
    in case _memberHostsRole rhg of
         RoleInRel' RoleTplt ->
           "Rels in which it is the Tplt"
         RoleInRel' (RoleMember (n :: Int)) ->
           let mbrs = either (const noMiscount) id
                      $ replaceNth (Phrase $ "it") n
                      $ replicate (arity tplt) $ Phrase "_"
           in either (const noLeft) id $
              eParenShowExpr 3 noRslt $ ExprRel $
              Rel mbrs $ ExprTplt tplt
         _ -> error "-- TODO ? why does this never seem to happen?"

-- | Shows the label of the group, not its members.
instance ShowColor ViewOptions RelHostGroup where
  -- PITFALL: Egregious duplication; see `Show` instance.
  showColor _ rhg = let
    tplt :: Tplt Expr = _memberHostsTplt rhg
    noLeft     = error "show RelHostGroup: impossible"
    noRslt     = error "show RelHostGroup: Rslt irrelevant"
    noMiscount = error "show RelHostGroup: Did I miscount?"
    in case _memberHostsRole rhg of
         RoleInRel' RoleTplt ->
           [ ("Rels in which it is the Tplt", TextColor) ]
         RoleInRel' (RoleMember (n :: Int)) ->
           let mbrs = either (const noMiscount) id
                 $ replaceNth (Phrase $ "it") n
                 $ replicate (arity tplt) $ Phrase "_"
           in either (const noLeft) id $
              eParenShowColorExpr 3 noRslt $ ExprRel $
              Rel mbrs $ ExprTplt tplt
         _ -> error "-- TODO ? why does this never seem to happen?"

data ViewForkType  -- ^ used to group a set of related VenExprs
  = VFQuery ViewQuery -- ^ groups the results of searching for the ViewQuery
  | VFMembers -- ^ groups the members of a Rel
  | VFTpltHosts -- ^ groups the Tplts that use an Expr
  | VFRelHosts RelHostGroup -- ^ groups the Rels in which an Expr appears
  | VFSearch -- ^ groups the results of running an Expr as a search
  deriving (Eq, Ord, Show)
makePrisms ''ViewForkType

data ViewFork = ViewFork
  { _viewForkCenter :: Maybe Addr -- ^ the address of its view-parent
  , _viewForkSortTplt :: Maybe TpltAddr -- ^ how to sort its view-children
  , _viewForkType :: ViewForkType }
  deriving (Eq, Ord, Show)
makeLenses ''ViewFork

-- | = A `ViewExprNode` is a node in a tree of descendents of search results.
--
-- Each search returns a flat list of `ViewExprNode`s.
-- The user can then choose to view members and hosts of any node,
-- recursively, thus building a "view tree".
-- Nodes usually have a "view-parent", and sometimes have "view-children".
--
-- A `VenFork` announces the relationship
-- between its parent in the view tree and its children.
--
-- PITFALL: `PTree ViewExprNode` permits invalid state.
-- A `VenFork` containing a `VQuery` should be top of buffer, nowhere else.
-- A `VenFork`'s children should only be `VenExpr`s, and vice-versa.

data ViewExprNode  -- ^ the primary objects in a view of search results
  = VenExpr ViewExpr -- ^ represents a member of the Rslt
  | VenFork ViewFork -- ^ used to group a set of related VenExprs
  deriving (Eq, Ord, Show)
makePrisms ''ViewExprNode

-- | augments a `ViewExprNode` with information needed to draw it.
data ExprRow = ExprRow {
    _viewExprNode :: ViewExprNode
  , _numColumnProps  :: NumColumnProps
  , _otherProps   :: OtherProps
  } deriving (Show, Eq, Ord)
makeLenses ''ExprRow

instance ShowBrief ViewExprNode where
  showBrief (VenExpr x) =
    show (x ^. viewExpr_Addr) ++ ": "
    ++ unColorString (x ^. viewExpr_String)
  showBrief (VenFork vf) = case _viewForkType vf of
    VFQuery (QueryView s) -> show s
    VFQuery CycleView -> "Cycle detected! Please break it somewhere."
    VFMembers -> "its members"
    VFTpltHosts -> "Tplts using it as a separator"
    VFRelHosts r -> show r
    VFSearch -> "search results"

instance ShowColor ViewOptions ViewExprNode where
  showColor vo (VenExpr ve) =
    ( if _viewOpt_ShowAddresses vo
      then [(show (_viewExpr_Addr ve) ++ " ", AddrColor)]
      else [] )
    ++ _viewExpr_String ve
  showColor vo f@(VenFork vf) = case _viewForkType vf of
    VFRelHosts r -> showColor vo r
    _ -> [(showBrief f, TextColor)]

exprTree_focusAddr :: PTree ExprRow -> Either String Addr
exprTree_focusAddr =
  (\case VenExpr rv -> Right $ rv ^. viewExpr_Addr
         _         -> Left $ "Can only be called from a VenExpr." )
  . (^. pTreeLabel . viewExprNode)
