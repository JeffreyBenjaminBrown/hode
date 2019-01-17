module Types where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S


-- | The type variables `sp` and `e` below stand for a space and the
-- elements of that space. They could be reified via womething like this:
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
--class Space e sp | sp -> e
--instance Space e (Graph e)


type Var = String
  -- ^ When a `Query` creates a `Var`, the result has no `varDets`.
  -- However, sometimes a Var is created by subsetting an earlier one.
  -- In that case, suppose it decomposes as `v@(Var _ (source, dets))`.
  -- "source" is the earlier Var, and "dets" is a set of variables
  -- that were calculated based on source's earlier calculation.

data Source = Source  { sourceVar :: Var }
            | Source' { sourceVar :: Var
                      , dets :: (Set Var) }

data Find e sp = Find { findFunction          :: sp -> Subst e -> Set e
                      , findDets              :: Set Var }
  -- ^ If `findFunction` doesn't use the `Subst`, `findDets` should be empty.
data Test e sp = Test {  testFunction         :: sp -> Subst e ->    e -> Bool
                       , testDets             :: Set Var }
  -- ^ If `condFunction` doesn't use the `Subst`, `condDeps` should be empty.
data VarTest e sp = VarTest { varTestFunction :: sp -> Subst e         -> Bool
                            , varTestDets     :: Set Var }
  -- ^ If `*Function` doesn't use the `Subst`, `*Dets` should be empty.

data Query e sp = QFind    (Find    e sp)
                | QTest    (Test    e sp)
                | QVarTest (VarTest e sp)
                | QJunct   (Junction   e sp)
                | QQuant   (Quantifier e sp)

data Junction e sp = And {clauses :: [Query e sp] } -- ^ order not important
                   | Or  {clauses :: [Query e sp] } -- ^ order not important

data Quantifier e sp =
    ForAll  { name :: Var, source :: Source, goal :: Query e sp }
  | ForSome { name :: Var, source :: Source, goal :: Query e sp }

type Subst e    = Map Var e
type CondElts e = Map e (Set (Subst e))
  -- ^ The set of solutions to a query: which `Elts` solve it, and which
  -- values of earlier-computed input variables permit each solution.
  -- Each Subst is a set of determinants leading to the associated Elt.
  -- Uses `Set` because multiple `Subst`s might obtain the same `Elt`.
  -- ^ PITFALL: If `Elt` is possible without any determining bindings, then
  -- the `Set` should include an empty `Map`. The `Set` should not be empty.
type Possible e = Map Var (CondElts e)
