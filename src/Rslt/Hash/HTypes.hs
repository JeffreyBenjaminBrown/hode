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
    HIt -- ^ Indicates what sub-expression the host `HExpr` is searching for.
  -- `HExpr`s with no `HIt` return whatever matches the top-level expression.
  | HEval HExpr -- ^ Evaluates the `It`(s) in its sub-`HExpr`.
  -- (Including multiple `It` sub-`HExpr`s is kind of weird, but legal.)
  | HExpr Expr -- ^ When you want just one `Expr`. Note that the `ExprAddr`
  -- constructor permits referring to an `Expr` by its `Addr`.
  | HMap (Map Role HExpr) -- ^ The search workhorse.
  -- Must specify an HExpr to match at least one Role.
  | HAnd [HExpr]      -- ^ Intersection.
  | HOr  [HExpr]      -- ^ Union. Pronounced "a chore".
  | HDiff HExpr HExpr -- ^ Set difference.
  deriving (Eq, Ord, Show)
