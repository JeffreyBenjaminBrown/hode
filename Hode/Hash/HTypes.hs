-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

module Hode.Hash.HTypes where

import Data.Functor.Foldable.TH
import Data.Map (Map)

import Hode.Qseq.QTypes
import Hode.Rslt.RTypes


type Level = Int
type Joint = String

-- | An `HExpr` describes a set (maybe empty) of `Expr`s in a `Rslt`.
data HExpr =
    HExpr Expr  -- ^ When you want exactly one `Expr`, and know which.
  -- The `Addr` constructor permits referring to an `Expr` by its `Addr`.
  | HMap  HMap  -- ^ The search workhorse.
  | HEval HExpr [RolePath] -- ^ Finds matches to the `HExpr`, then retrieves
  -- from each match the subexpression each `RolePath` arrives at.
  -- (Inclduing more than one path in the `[RolePath]` is weird but legal.)
  | HVar  Var         -- ^ To look up the `Var` from a `Subst Addr Rslt`.
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
type HMap = Map Role HExpr


-- | = For parsing an HExpr

data PExpr = -- ^ intermediate type, on the way to parsing a `Rel`
    PExpr Expr
  | PMap PMap
  | PEval PExpr
  | PVar Var
  | PDiff PExpr PExpr
  | PAnd [PExpr]
  | POr [PExpr]
  | Any
  | It (Maybe PExpr)
  | PRel PRel
   deriving (Eq, Show)

type PMap = Map Role PExpr


data PRel -- ^ intermediate type, on the way to parsing a `Rel`
   = Absent -- ^ The leftmost and rightmost members of an `Open` or
     -- `Closed` might be absent. Interior ones should not be.
   | Closed     [PRel] [Joint] -- ^ First list: members. Second: joints.
   -- Only the first and last members can be Absent. |joints| = |members| - 1
   | Open Level [PRel] [Joint] -- ^ Like `Closed`, but more things
   -- might be inserted into it.
   | PNonRel PExpr
   deriving (Eq, Show)

makeBaseFunctor ''HExpr
makeBaseFunctor ''PExpr
makeBaseFunctor ''PRel
