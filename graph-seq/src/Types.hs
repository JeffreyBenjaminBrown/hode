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

data Var = Var { varName :: String }
  -- ^ When a `Query` creates a `Var`, the result has no `varDets`.
  -- However, sometimes a Var is created by subsetting an earlier one.
  -- In that case, suppose it decomposes as `v@(Var _ (source, dets))`.
  -- "source" is the earlier Var, and "dets" is a set of variables
  -- that were calculated based on source's earlier calculation.
  deriving (Show, Eq, Ord)

data Source = Source Var -- ^ draw from the solution set of a prior Query
            | Source' { name :: Var
                      , dets :: (Set Var) }

data Find = Find { findFunction :: Data -> Subst -> Set Elt
                 , findDets     :: Set Var }
  -- ^ If `findFunction` doesn't use the `Subst`, `findDets` should be empty.
data Test = Test { testFunction :: Data -> Subst ->     Elt -> Bool
                 , testDets     :: Set Var }
  -- ^ If `condFunction` doesn't use the `Subst`, `condDeps` should be empty.
data VarTest = VarTest { varTestFunction :: Data -> Subst -> Bool
                       , varTestDets     :: Set Var }
  -- ^ If `*Function` doesn't use the `Subst`, `*Dets` should be empty.

data Query = QFind Find
           | QTest Test
           | QVarTest VarTest
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
type Possible = Map Var CondElts
