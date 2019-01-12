module Types where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S


data Graph = Graph {
    graphNodes    :: Set Elt             -- ^ good for disconnected graphs
  , graphChildren :: Map Elt (Set Elt)   -- ^ keys are parents
  , graphParents  :: Map Elt (Set Elt) } -- ^ keys are children
  deriving (Show, Eq, Ord)
type Elt = Int    -- eventually Int   will be replaced with Expr
type Data = Graph -- eventually Graph will be replaced with Rslt

data Var = Var { varName :: String
               , varDets :: Set Var } -- ^ The determinants
  -- of a Var are variables that were calculated based on its own
  -- earlier calculation. If the determinants are already bound, that
  -- restricts the possible values the Var can take.
  deriving (Eq, Ord)
instance Show Var where
  show v = "Var " ++ varName v ++ "<=" ++ show (S.toList $ varDets v)

data Find = Find { findFunction :: Data -> Subst -> Set Elt
                 , findDets     :: Set Var }
  -- ^ If `findFunction` doesn't use the `Subst`, `findDets` should be empty.
data Test = Test { testFunction :: Data -> Subst ->     Elt -> Bool
                 , testDeps     :: Set Var }
  -- ^ If `condFunction` doesn't use the `Subst`, `condDeps` should be empty.

data Query = QFind Find
           | QTest Test
           | QAnd           [Query] -- ^ order not important
           | QOr            [Query] -- ^ order not important
           | ForAll  Var Query
           | ForSome Var Query

type Subst    = Map Var Elt
type CondElts = Map Elt (Set Subst)
  -- ^ The set of solutions to a query: which `Elts` solve it, and which
  -- values of earlier-computed input variables permit each solution.
  -- Each Subst is a set of determinants leading to the associated Elt.
  -- Uses `Set` because multiple `Subst`s might obtain the same `Elt`.
  -- ^ PITFALL: If `Elt` is possible without any determining bindings, then
  -- the `Set` should include an empty `Map`. The `Set` should not be empty.
type Possible  = Map Var CondElts
type Program   = Data
               -> [(Var, Query)] -- ^ queries can depend on earlier ones
               -> Possible
