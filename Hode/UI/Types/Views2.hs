{-# LANGUAGE
ScopedTypeVariables,
MultiParamTypeClasses,
TemplateHaskell,
ViewPatterns
#-}

module Hode.UI.Types.Views2 (
    RelHosts'(..), memberHostsRole', memberHostsTplt'
  , ViewForkType'(..), _VFQuery', _VFMembers', _VFTpltHosts'
    , _VFRelHosts', _VFQuery'
  , ViewFork'(..), viewForkCenter', viewForkSortTplt', viewForkType'
  , ViewExprNode'(..), _VExpr', _VFork'
  ) where

import Control.Lens

import Hode.Brick
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Rslt.ShowColor
import Hode.UI.Types.Views
import Hode.Util.Misc


data RelHosts' = RelHosts' { _memberHostsRole'   :: Role
                           , _memberHostsTplt'   :: Tplt Expr }
               deriving (Eq, Ord)
makeLenses ''RelHosts'

-- | Shows the label of the group, not its members.
instance Show RelHosts' where
  -- PITFALL: Egregious duplication; see `ShowColor` instance.
  show rhg = let
    tplt :: Tplt Expr = _memberHostsTplt' rhg
    noLeft     = error "show RelHosts: impossible"
    noRslt     = error "show RelHosts: Rslt irrelevant"
    noMiscount = error "show RelHosts: Did I miscount?"
    RoleInRel' (RoleMember (n :: Int)) =
      _memberHostsRole' rhg
    mbrs = either (const noMiscount) id
           $ replaceNth (Phrase $ "it") n
           $ replicate (arity tplt) $ Phrase "_"
    in either (const noLeft) id $
       eParenShowExpr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

-- | Shows the label of the group, not its members.
instance ShowColor ViewOptions RelHosts' where
  -- PITFALL: Egregious duplication; see `Show` instance.
  showColor _ rhg = let
    tplt :: Tplt Expr = _memberHostsTplt' rhg
    noLeft     = error "show RelHosts: impossible"
    noRslt     = error "show RelHosts: Rslt irrelevant"
    noMiscount = error "show RelHosts: Did I miscount?"
    RoleInRel' (RoleMember (n :: Int)) =
      _memberHostsRole' rhg
    mbrs = either (const noMiscount) id
           $ replaceNth (Phrase $ "it") n
           $ replicate (arity tplt) $ Phrase "_"
    in either (const noLeft) id $
       eParenShowColorExpr 3 noRslt $ ExprRel $
       Rel mbrs $ ExprTplt tplt

data ViewForkType'  -- ^ used to group a set of related VExprs
  = VFQuery' ViewQuery -- ^ groups the results of searching for the ViewQuery
  | VFMembers' -- ^ groups the members of a Rel
  | VFTpltHosts' -- ^ groups the Tplts that use an Expr
  | VFRelHosts' RelHosts' -- ^ groups the Rels in which an Expr appears
  | VFSearch' -- ^ groups the results of running an Expr as a search
  deriving (Eq, Ord, Show)
makePrisms ''ViewForkType'

data ViewFork' = ViewFork'
  { _viewForkCenter' :: Maybe Addr -- ^ the address of its view-parent
  , _viewForkSortTplt' :: TpltAddr -- ^ how to sort its view-children
  , _viewForkType' :: ViewForkType' }
  deriving (Eq, Ord, Show)
makeLenses ''ViewFork'

data ViewExprNode'  -- ^ the primary objects in a view of search results
  = VExpr' ViewExpr -- ^ represents a member of the Rslt
  | VFork' ViewFork' -- ^ used to group a set of related VExprs
  deriving (Eq, Ord, Show)
makePrisms ''ViewExprNode'

instance ShowBrief ViewExprNode' where
  showBrief (VExpr' x) =
    show (x ^. viewExpr_Addr) ++ ": "
    ++ unColorString (x ^. viewExpr_String)
  showBrief (VFork' vf) = case _viewForkType' vf of
    VFQuery' (QueryView s) -> show s
    VFQuery' CycleView -> "Cycle detected! Please break it somewhere."
    VFMembers' -> "its members"
    VFTpltHosts' -> "Tplts using it as a separator"
    VFRelHosts' r -> show r
    VFSearch' -> "search results"

instance ShowColor ViewOptions ViewExprNode' where
  showColor vo (VExpr' ve) =
    ( if _viewOpt_ShowAddresses vo
      then [(show (_viewExpr_Addr ve) ++ " ", AddrColor)]
      else [] )
    ++ _viewExpr_String ve
  showColor vo f@(VFork' vf) = case _viewForkType' vf of
    VFRelHosts' r -> showColor vo r
    _ -> [(showBrief f, TextColor)]
