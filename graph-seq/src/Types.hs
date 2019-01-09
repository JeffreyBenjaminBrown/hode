module Types where

import Data.Map
import Data.Set


data Graph = Graph { children :: Map Elt (Set Elt)   -- ^ keys are parents
                   , parents  :: Map Elt (Set Elt) } -- ^ keys are children
  deriving (Show, Eq, Ord)
type Elt = Int    -- eventually Int   will be replaced with Expr
type Data = Graph -- eventually Graph will be replaced with Rslt

data Var = Var { varTarget :: String
               , varDets   :: Set Var } -- ^ The determinants
  -- of a Var are variables that were calculated based on its own
  -- earlier calculation. If the determinants are already bound, that
  -- restricts the possible values the Var can take.
  deriving (Show, Eq, Ord)

data Find = Find { findFunction :: Data -> Subst -> Set Elt
                 , findDeps     :: Set Var }
  -- ^ If `findFunction` doesn't use the `Subst`, `findDeps` should be empty.
data Cond = Cond { condFunction :: Data -> Subst ->     Elt -> Bool
                 , condDeps     :: Set Var }
  -- ^ If `condFunction` doesn't use the `Subst`, `condDeps` should be empty.

data Query = QFind Find
           | QCond Cond
           | QAnd           [Query] -- ^ order not important
           | QOr            [Query] -- ^ order not important
           | ForAll  Var Query
           | ForSome Var Query

type Subst    = Map Var Elt
type CondElts = Map Elt (Set Subst)
  -- ^ Uses `Set` because multiple `Subst`s might obtain the same `Elt`.
  -- PITFALL: If Elt is possible without any other bindings, then
  -- the `Set` should include `M.empty`. If the `Set` is `S.empty`,
  -- it is as if that `Elt` is not in the `Map`.
type Possible  = Map Var CondElts
  -- ^ Describes the conditions under which a `Var` could be any of
  -- the keys of its associated `CondElts`.
type Program   = Data
               -> [(Var, Query)] -- ^ queries can depend on earlier ones
               -> Possible
