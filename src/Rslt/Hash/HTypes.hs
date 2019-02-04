module Rslt.Hash.HTypes where

import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.RTypes


-- | An expression in the Hash language.
-- Describes a (possibly empty) collection of `Expr`s in a `Rslt`.
data HExpr =
    HMap  HMap -- ^ The search workhorse.
  | HEval HMap -- ^ Evaluates the `It`(s) in its `HMap`.
  -- (Including multiple `It`s in an `HMap` is legal, if kind of weird.)
  | HExpr  Expr   -- ^ When you want exactly one `Expr`, and know which.
  -- The `ExprAddr` constructor permits referring to an `Expr` by its `Addr`.
  | HDiff HExpr HExpr -- ^ Set difference.
  | HAnd [HExpr]      -- ^ Intersection.
  | HOr  [HExpr]      -- ^ Union. Pronounced "a chore".
  deriving (Eq, Ord, Show)

-- | An `HMap` m is used to request all expressions x such that for each
-- key r in m, such that r is mapped to h, some expression in the
-- result of searching for h appears in position r in x.
-- This is not type-enforced, but to be valid an HMap  must be nonempty.
-- Searches that find nothing are easily specified -- for instance,
-- if expr x is never the second member of anything, then the `HMap`
-- `M.singleton (RoleMember 2) x` will, appropriately, find nothing.
--
-- The `Left HIt` values are ignored when evaluating the `HMap`;
-- they come into play when the `HMap` is a subexpression of some `HEval`.
type HMap = Map Role (Either HIt HExpr)


-- | Indicates what sub-expression the host `HMap` is searching for.
-- An `HExpr` with no `HIt` returns whatever matches the top-level expression.
data HIt = HIt
  deriving (Eq, Ord, Show)
