module Types where

import Data.Map
import Data.Set


data Data = Graph { children :: Map Int Int   -- ^ keys here are parents
                   , parents  :: Map Int Int } -- ^ keys here are children
  deriving (Show, Eq, Ord)

newtype Var = Var String deriving (Show, Eq, Ord)
data VarFunc = VarFunc {
  varName :: Var
  , varDets :: Set Var }
  deriving (Show, Eq, Ord)

type Find = Data -> Subst -> Set Int
type Cond = Data -> Subst ->     Int -> Bool
data Query = QFind Find
           | QCond Cond
           | QIntersect [Query]
           | QUnion     [Query]
           | ForAll  [VarFunc] Query
           | ForSome [VarFunc] Query

type Subst = Map Var Int
-- | `VarResult` is used for partial as well as complete results.
--
-- `VarResult` example: suppose that in the VarResult R, Y could be
-- bound to 1 under either of the following conditions:
--     when X is bound to 2
--     when X is bound to 3 and Z is bound to 4
-- In that case, M.lookup 1 (M.lookup Y R) =
--     S.fromList [ S.fromList [ (X,2)        ]
--                , S.fromList [ (X,3), (Z,4) ] ]
type Result = Map Var (Map Int (Set Subst))
type Program = Data -> [ (VarFunc, Query) ] -> Result
