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
    HVar HVar -- TODO ? Flatten: delete HVar type, promote constructors.
  | HEval HExpr -- ^ Evaluates the `It` in its sub-`HExpr`.
  | HAddr Addr -- ^ When you want just one `Expr`, and know where it is.
  | HExpr Expr -- ^ When you want just one `Expr`.
  | HMap (Map Role HExpr) -- ^ The search workhorse.
  | HAnd [HExpr]      -- ^ Intersection.
  | HOr  [HExpr]      -- ^ Union. Pronounced "a chore".
  | HDiff HExpr HExpr -- ^ Set difference.

-- | A variable in the Hash language.
data HVar =
    It  -- ^ Indicates what sub-expression the host `HExpr` is searching for.
        -- An `HExpr` with no `It` returns matches to the top expression.
  | Any -- ^ A wildcard; matches anything.
