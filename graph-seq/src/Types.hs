module Types where

import Data.Map
import Data.Set


data Find = Find
data Cond = Cond

data Graph = Graph { children :: Map Int Int   -- ^ keys are parents
                   , parents  :: Map Int Int } -- ^ keys are children

newtype Var = Var String

data Program = Program { programGraph  :: Graph
                       , programOutput :: [Var]
                       , program       :: [(Var,Query)] }

data Query = QFind Find
           | QCond Cond
           | QAnd [Query]
           | QOr [Query]
           | ForAll [Var] Query
           | ForSome [Var] Query

type Binding = (Var, Int)

-- | Note that the empty set of bindings is a valid value.
type VarResult = (Var, Map Int (Set Binding))
