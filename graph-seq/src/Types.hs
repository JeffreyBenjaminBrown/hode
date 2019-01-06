module Types where

import Data.Map
import Data.Set


type Elt = Int
data Data = Graph { children :: Map Elt (Set Elt)   -- ^ keys are parents
                  , parents  :: Map Elt (Set Elt) } -- ^ keys are children
  deriving (Show, Eq, Ord)

newtype Var = Var String deriving (Show, Eq, Ord)
data VarFunc = VarFunc {
    varFuncName ::     Var
  , varFuncDets :: Set Var }
  deriving (Show, Eq, Ord)
type Subst = Map Var Elt

type Find = Data -> Subst -> Set Elt
type Cond = Data -> Subst ->     Elt -> Bool
data Query = QFind Find
           | QCond Cond
           | QAnd                 [Query] -- ^ order not important
           | QOr                  [Query] -- ^ order not important
           | ForAll  (Set VarFunc) Query
           | ForSome (Set VarFunc) Query

-- | `Result` is used for partial as well as complete results.
--
-- `Result` example: suppose that in the `Result` R, Y could be
-- bound to 1 under either of the following conditions:
--     when X is bound to 2
--     when X is bound to 3 and Z is bound to 4
-- In that case, M.lookup 1 (M.lookup Y R) =
--     S.fromList [ S.fromList [ (X,2)        ]
--                , S.fromList [ (X,3), (Z,4) ] ]
type Result = Map Var (Map Elt (Set Subst))
type Program = Data
             -> [ (VarFunc, Query) ]  -- ^ order important
             -> Result
