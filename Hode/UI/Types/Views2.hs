{-# LANGUAGE
ScopedTypeVariables,
LambdaCase,
MultiParamTypeClasses,
TemplateHaskell,
ViewPatterns
#-}

module Hode.UI.Types.Views2 (
  -- * types and optics
    RelHostGroup(..), memberHostsRole, memberHostsTplt
  , ViewForkType(..), _VFQuery, _VFMembers, _VFTpltHosts
    , _VFRelHosts, _VFSearch
  , ViewFork(..), viewForkCenter, viewForkSortTplt, viewForkType
  , ViewExprNode(..), _VenExpr, _VenFork
  , ExprRow(..), viewExprNode, columnProps, otherProps

  -- * misc
  , exprTree_focusAddr -- ^ PTree ExprRow -> Either String Addr
  ) where

import Control.Lens

import Hode.Brick
import Hode.PTree.Initial
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Rslt.ShowColor
import Hode.UI.Types.Views
import Hode.Util.Misc


data RelHostGroup = RelHostGroup { _memberHostsRole   :: Role
                           , _memberHostsTplt   :: Tplt Expr }
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
    RoleInRel' (RoleMember (n :: Int)) =
      _memberHostsRole rhg
    mbrs = either (const noMiscount) id
           $ replaceNth (Phrase $ "it") n
           $ replicate (arity tplt) $ Phrase "_"
    in either (const noLeft) id $
       eParenShowExpr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

-- | Shows the label of the group, not its members.
instance ShowColor ViewOptions RelHostGroup where
  -- PITFALL: Egregious duplication; see `Show` instance.
  showColor _ rhg = let
    tplt :: Tplt Expr = _memberHostsTplt rhg
    noLeft     = error "show RelHostGroup: impossible"
    noRslt     = error "show RelHostGroup: Rslt irrelevant"
    noMiscount = error "show RelHostGroup: Did I miscount?"
    RoleInRel' (RoleMember (n :: Int)) =
      _memberHostsRole rhg
    mbrs = either (const noMiscount) id
           $ replaceNth (Phrase $ "it") n
           $ replicate (arity tplt) $ Phrase "_"
    in either (const noLeft) id $
       eParenShowColorExpr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

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

data ViewExprNode  -- ^ the primary objects in a view of search results
  = VenExpr ViewExpr -- ^ represents a member of the Rslt
  | VenFork ViewFork -- ^ used to group a set of related VenExprs
  deriving (Eq, Ord, Show)
makePrisms ''ViewExprNode

-- | augments a `ViewExprNode` with information needed to draw it.
data ExprRow = ExprRow {
    _viewExprNode :: ViewExprNode
  , _columnProps  :: ColumnProps
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
