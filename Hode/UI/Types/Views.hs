{-# LANGUAGE
ScopedTypeVariables,
MultiParamTypeClasses,
TemplateHaskell,
ViewPatterns
#-}

module Hode.UI.Types.Views (
    ViewOptions(..), viewOpt_ShowAddresses, viewOpt_ShowAsAddresses
  , viewOpt_WrapLength
  , ColumnProps
  , OtherProps(..), folded
  , ViewQuery(..), _QueryView, _CycleView
  , ViewExpr(..), viewExpr_Addr, viewExpr_showAsAddrs, viewExpr_String
  ) where

import Control.Lens hiding (folded)
import Data.Set (Set)
import Data.Map (Map)

import Hode.Brick
import Hode.Hash.HTypes
import Hode.Rslt.RTypes


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

type ColumnProps = Map HExpr Int

data OtherProps = OtherProps {
  _folded :: Bool -- ^ whether the ViewExprNodes children are hidden
  } deriving (Show, Eq, Ord)
makeLenses ''OtherProps

-- | (Usually) what the user searched for.
-- Should be the top of every PTree of ViewExprs.
data ViewQuery
  = QueryView String -- ^ the String is what was searched for
  | CycleView -- ^ When Hode finds a cycle, it makes one of these.
  deriving (Eq, Ord, Show)
-- PITFALL: These `Prism`s have to be defined *right here*.
makePrisms ''ViewQuery

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
