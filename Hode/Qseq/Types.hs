module Hode.Qseq.Types where

import           Data.Map (Map)
import           Data.Set (Set)


-- | The type variables `sp` and `e` below stand for a space and the
-- elements of that space. They could be reified via womething like this:
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
--class Space e sp | sp -> e
--instance Space e (Graph e)


-- | When a `Query` creates a `Var`, the result has no `varDets`.
-- However, sometimes a Var is created by subsetting an earlier one.
-- In that case, suppose it decomposes as `v@(Var _ (source, dets))`.
-- "source" is the earlier Var, and "dets" is a set of variables
-- that were calculated based on source's earlier calculation.
data Var = VarString String -- ^ user-defined variables
         | VarRowNode -- ^ a builtin variable representing
                      -- "the item shown in some row of some buffer"
    deriving (Show, Eq, Ord)

data Query e sp = QFind  (Find       e sp)
                | QTest  (Test       e sp)
                | QVTest (VarTest    e sp)
                | QJunct (Junction   e sp)
                | QQuant (Quantifier e sp)


-- | `Find`, `Test` and `VarTest` are the atomic `Query`s,
-- of which more complex ones are made.
--
-- A `Find e sp` returns a `Set e` from a space of type sp.
-- A `Test e sp` filters the results of a `Find`.
-- A `VarTest e sp` filters the `Set (Subst e)` produced by some
-- super-`QQuant` (in the sense of superset).
--
-- Every `Query` must have at least one `Find`, but it needs neither
-- `Test`s nor `VarTest`s.
--
-- Any `VarTest`s are run first.
-- Then `Find`s are run using the surviving `Subst`s.
-- Finally, `Test`s are run to filter the results of the `Find`s.
--
-- For instance, in the following `Query`:
-- > QQuant $ ( ForAll "a" "as"
-- >            ( QJunct $ And [ QFind $ findChildren $ Right "a"
-- >                           , QTest $ test (<) $ Left 5 ] )
-- >            [ QVTest $ mkVTestCompare (<) (Left 0) (Right "a") ] )
-- the `Subst` mapping a to aVal is tried for all values of aVal in as,
-- and then filtered so that only values of a greater than 0 survive.
-- For those remaining `Subst`s, all the children of "a" are retrieved,
-- and then filtered to include only values greater than 5.
-- Those children which, for *all* values of a which pass the `VarTest`,
-- are found by the `Find` and pass the `Test`, are the `Query`'s results.

data Find e sp    = Find {
    findFunction    :: sp ->               Subst e ->    Either String (Set e)
  , findUses        :: Set Var }
  -- ^ If `findFunction` doesn't use the `Subst`, `findUses` should be empty.
data Test e sp    = Test {
    testFunction    :: sp ->               Subst e -> e -> Either String Bool
  , testUses        :: Set Var }
  -- ^ If `condFunction` doesn't use the `Subst`,`condDeps` should be empty.
data VarTest e sp = VarTest {
    varTestFunction :: sp -> Possible e -> Subst e ->      Either String Bool
  , varTestUses     :: Set Var }
  -- ^ If `*Function` doesn't use the `Subst`, `varTestUses` should be empty.
  -- PITFALL : The dependencies in the `varTestUses` field refer to
  -- `Var`s in the `Subst`, not keys in the `Possible`. But also it's
  -- academic, as (currently) dependencies from a `VarTest` are, unlike
  -- those from a `Find` or a `Test`, not recorded as inputs to the result.

data Junction e sp = QAnd {clauses :: [Query e sp] } -- ^ order not important
                   | QOr  {clauses :: [Query e sp] } -- ^ order not important

data Quantifier e sp =
    ForAll  { name :: Var, source :: Var
            , _condition :: Query e sp -- ^ PITFALL: partial function.
                                         , goal :: Query e sp
              -- `conditions` is a total version.
            }
  -- ^ The `_conditions` field lets you narrow the possibilities considered.
  -- Rather than requiring all x to satisfy z, you might want to require
  -- the less strict condition that all x which satisfy y satisfy z.
  --  In that case, you would put y in the `conditions` field.
  -- TODO : the `_conditions` list is currently treated like an And.
  -- It ought simply to be a (`varTestLike`) `Query`, not a list of them.
  | ForSome { name :: Var, source :: Var , goal :: Query e sp }

-- | `conditions` called on a `ForSome` returns the empty `QAnd`, which
-- is effectively always `True` -- thus imposing no conditions.
condition :: Quantifier e sp -> Query e sp
condition   (ForSome _ _ _)  = QJunct $ QAnd []
condition q@(ForAll _ _ _ _) = _condition q

type Subst e    = Map Var e
type CondElts e = Map e (Set (Subst e))
  -- ^ The set of solutions to a query: which `Elts` solve it, and which
  -- values of earlier-computed input variables permit each solution.
  -- Each Subst is a set of determinants leading to the associated Elt.
  -- Uses `Set` because multiple `Subst`s might obtain the same `Elt`.
  -- ^ PITFALL: If `Elt` is possible without any determining bindings, then
  -- the `Set` should include an empty `Map`. The `Set` should not be empty.
type Possible e = Map Var (CondElts e)
