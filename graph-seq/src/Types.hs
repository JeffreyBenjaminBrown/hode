module Types where

import Data.Map
import Data.Set


type Elt = Int
data Data = Graph { children :: Map Elt (Set Elt)   -- ^ keys are parents
                  , parents  :: Map Elt (Set Elt) } -- ^ keys are children
  deriving (Show, Eq, Ord)

-- TODO remove the Var type, rename VarFunc -> Var, make target a String
newtype Var = Var String deriving (Show, Eq, Ord)
data VarFunc = VarFunc { varFuncTarget ::     Var
                       , varFuncDets   :: Set VarFunc } -- ^ The determinants
  -- of a VarFunc are variables that were calculated based on its own
  -- earlier calculation. If the determinants are already bound, that
  -- restricts the possible values the VarFunc can take.
  deriving (Show, Eq, Ord)

data Find = Find { findFunction :: Data -> Subst -> Set Elt
                 , findDeps     :: Set VarFunc }
  -- ^ If `findFunction` doesn't use the `Subst`, `findDeps` should be empty.
data Cond = Cond { condFunction :: Data -> Subst ->     Elt -> Bool
                 , condDeps     :: Set VarFunc }
  -- ^ If `condFunction` doesn't use the `Subst`, `condDeps` should be empty.

data Query = QFind Find
           | QCond Cond
           | QAnd           [Query] -- ^ order not important
           | QOr            [Query] -- ^ order not important
           | ForAll  VarFunc Query
           | ForSome VarFunc Query

type Subst    = Map VarFunc Elt
type CondElts = Map Elt (Set Subst)
  -- ^ Uses `Set` because multiple `Subst`s might obtain the same `Elt`.
  -- PITFALL: If Elt is possible without any other bindings, then
  -- the `Set` should include `M.empty`. If the `Set` is `S.empty`,
  -- it is as if that `Elt` is not in the `Map`.
type Possible1 = (Var, CondElts)
  -- ^ Describes the conditions under which the `Var` could be any of
  -- the keys of the `CondElts`.
type Possible  = Map VarFunc CondElts
type Program   = Data
               -> [(VarFunc, Query)] -- ^ queries can depend on earlier ones
               -> Possible
