{-# LANGUAGE ScopedTypeVariables
, GADTs
, TemplateHaskell
, TypeFamilies
, DeriveFunctor, DeriveFoldable, DeriveTraversable
, ViewPatterns #-}

module Hode.Hash.HTypes where

import Data.Functor.Foldable.TH
import Data.Map (Map)

import Hode.Qseq.QTypes
import Hode.Rslt.RTypes


type Level = Int
type Joint = String

-- | Used to specify transitive searches, for binary `Tplt`s.
data SearchDir = SearchLeftward | SearchRightward
  deriving (Show, Eq, Ord)

-- | An `HExpr` describes a set (maybe empty) of `Expr`s in a `Rslt`.
data HExpr where
  -- GADT only for the sake of adding comments to constructor arguments
  HExpr :: Expr -> HExpr
  -- ^ When you want exactly one `Expr`, and know which.
  -- The `Addr` constructor permits referring to an `Expr` by its `Addr`.
  HMap :: HMap -> HExpr -- ^ The search workhorse.
  HEval :: HExpr -- ^ First, find matches to this.
        -> [RolePath] -- ^ Then, traverse each match along these paths,
        -- and return whatever each path leads to.
        -- (Using more than one path is weird but legal.)
        -- The `[Role]` argument to `HTrans` is used similarly.
        -> HExpr
  HVar ::  Var -> HExpr -- ^ To look up the `Var` from a `Subst Addr Rslt`.
  HDiff :: HExpr -> HExpr -> HExpr -- ^ Set difference.
  HAnd :: [HExpr] -> HExpr -- ^ Intersection.
  HOr ::  [HExpr] -> HExpr -- ^ Union.
  HReach :: SearchDir
         -> HExpr -- ^ template(s) to search along
                  -- (using more than one is weird but legal)
         -> HExpr -- ^ expression(s) to start from
         -> HExpr -- ^ every `Expr` that can be reached by traversing
         -- from the starting `Expr`s along the specified `Tplt`(s) in the specified direction
  HTrans :: SearchDir
    -> [Role] -- ^ The algorithm will find every pair (s,f)
    -- such that s is one of the starting `Expr`s, f is one of the ending `Expr`s, and `s` is transitively related to `f`. This list is then used to deterine what from those pairs to return -- either all the left members (`[RoleMember 1]`), all the right members (`[RoleMember 2]`), or both (`[RoleMember 1, RoleMember 2]`).
    -> HExpr -- ^ template(s) to search along
             -- (using more than one is weird but legal)
    -> HExpr -- ^ expression(s) to end at
    -> HExpr -- ^ expression(s) to start from
    -> HExpr
  deriving (Eq, Ord, Show)

-- | Example: if x is never the second member of anything, then the `HMap`
-- `M.singleton (RoleMember 2) x` will find nothing.
--
-- Definition:
-- An `HMap` m is used to request all expressions x such that for each
-- key r in m, such that r is mapped to h, some expression in the
-- result of searching for h appears in position r in x.
-- This is not type-enforced, but to be valid an HMap must be nonempty.
-- Searches that find nothing are easily specified -- for instance,
type HMap = Map Role HExpr


-- | = For parsing an HExpr

data PExpr = -- ^ intermediate type, on the way to parsing an `HExpr`
    PExpr Expr
  | PMap PMap
  | PEval PExpr
  | PVar Var
  | PDiff PExpr PExpr
  | PAnd [PExpr]
  | POr [PExpr]
  | PReach PRel
  | Any
  | It (Maybe PExpr)
  | PRel PRel
   deriving (Eq, Show)

type PMap = Map Role PExpr


data PRel -- ^ intermediate type, on the way to parsing an `HExpr`
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
