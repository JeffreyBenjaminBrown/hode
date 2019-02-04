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
    HExpr Expr -- ^ When you want just one `Expr`. Note that the
  -- `ExprAddr` constructor permits referring to an `Expr` by its `Addr`.
  | HMap  HMap -- ^ The search workhorse.
  -- Must specify an HExpr to match at least one Role.
  | HEval HMap -- ^ Evaluates the `It`(s) in its `HMap`.
  -- (Including multiple `It`s in an `HMap` is legal, if kind of weird.)
  | HAnd [HExpr]      -- ^ Intersection.
  | HOr  [HExpr]      -- ^ Union. Pronounced "a chore".
  | HDiff HExpr HExpr -- ^ Set difference.
  deriving (Eq, Ord, Show)

type HMap = Map Role (Either HIt HExpr)

-- | Indicates what sub-expression the host `HMap` is searching for.
-- An `HExpr` with no `HIt` returns whatever matches the top-level expression.
data HIt = HIt
  deriving (Eq, Ord, Show)
