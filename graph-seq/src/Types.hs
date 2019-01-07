module Types where

import Data.Map
import Data.Set


type Elt = Int
data Data = Graph { children :: Map Elt (Set Elt)   -- ^ keys are parents
                  , parents  :: Map Elt (Set Elt) } -- ^ keys are children
  deriving (Show, Eq, Ord)

newtype Var = Var String deriving (Show, Eq, Ord)
data VarFunc = VarFunc { varFuncName ::     Var
                       , varFuncDets :: Set Var } deriving (Show, Eq, Ord)

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
           | ForAll  VarFunc Query
           | ForSome VarFunc Query

type Subst     = Map Var Elt
type ConditionedValues = Map Elt (Set Subst)
  -- ^ Uses `Set` because multiple `Subst`s might obtain the same `Elt`.
type Result    = Map Var ConditionedValues
type Program = Data
             -> [(VarFunc, Query)] -- ^ queries can depend on earlier ones
             -> Result
