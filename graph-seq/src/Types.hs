module Types where

import Data.Map
import Data.Set


data Graph = Graph { children :: Map Int Int   -- ^ keys here are parents
                   , parents  :: Map Int Int } -- ^ keys here are children
  deriving (Show, Eq, Ord)

newtype Var = Var String deriving (Show, Eq, Ord)
data Elt = Ground Int | VarElt Var -- TODO ? Use these in QFind, QCond.
  deriving (Show, Eq, Ord)
data VarFunc = VarFunc {
  varName :: Var
  , varDets :: Set Var }
  deriving (Show, Eq, Ord)

data Program = Program { programGraph   :: Graph
                       , programQueries :: [ (Var, Query) ]
                       , programOutput  :: [ VarFunc ] }

type Find = Graph -> Subst -> Set Int
type Cond = Graph -> Subst ->     Int -> Bool
data Query = QFind Find
           | QCond Cond
           | QNot  Cond
           | QIntersect [Query]
           | QUnion     [Query]
           | ForAll  [VarFunc] Query
           | ForSome [VarFunc] Query -- TODO ? process in terms of ForAll

type Subst = Map Var Int
-- | `VarResult` is used for partial as well as complete results.
--
-- `VarResult` example: suppose that in the VarResult R, Y could be
-- bound to 1 under either of the following conditions:
--     when X is bound to 2
--     when X is bound to 3 and Z is bound to 4
-- In that case, M.lookup 1 (M.lookup Y R) =
--     S.fromList [ S.singleton  (X,2)
--                , S.fromList [ (X,3), (Z,4) ] ]
type Result = Map Var (Map Int (Set Subst))
