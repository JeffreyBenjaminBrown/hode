{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S


-- | = A `Space` is something you can search.
-- `e` is for `element`
class Space e sp | sp -> e where
  spElem :: e -> sp -> Bool
instance Eq e => Space e [e] where
  spElem = elem
instance Eq e => Space e (Set e) where
  spElem = elem
instance Eq e => Space e (Map e b) where
  spElem e m = elem e $ M.keysSet m

newtype Var = Var String deriving (Show, Eq, Ord) -- ^ ala Prolog
type Subst e = Map Var e                          -- ^ ala Prolog
  -- TODO ? Should `Subst` permit equating different `Var`s? (Prolog's does.)
data Search e sp = Search {
  solved     :: (Set (e, Subst e)) -- ^ (solution, `Subst`) pairs.
    -- TODO (#fast) `solved` could be a map from `e` to `Set (Subst e)`
    -- See `Search.unify` for why.
  , unsolved :: Maybe (Query e sp)
  }
type Sol e = (e, Subst e) -- ^ solution to a search
data Search' e sp = Solved {sols :: (Set (Sol e))}
  | SolvedAnd {sols' :: (Set (Sol e)), unsolved' :: Query e sp}
    -- in `SolvedAnd` each `Sol` holds only if the `Query` also holds for it
  | SolvedOr  {sols' :: (Set (Sol e)), unsolved' :: Query e sp}
    -- in `SolvedOr` all `Sol`s hold, plus anything satisfying the `Query`

data Pending = Pending deriving (Show, Eq) -- ^ Search not yet completable.

data Query e sp  = QP (PQuery e sp)  |  QN (NQuery e sp)  |  QV Var
  |  QAnd [Query e sp]  |  QOr [Query e sp]
  -- Since `Query` has no `Ord` instance, keep queries in lists, not sets.
-- | Positive queries can be run, finding all matching elts in a space.
data PQuery e sp = QElt e  |  QFind (sp -> Set e)
-- | A negative query is a condition: testable, but too broad to search for.
data NQuery e sp = QNot (Query e sp)  |  QCond (sp -> e -> Bool)
