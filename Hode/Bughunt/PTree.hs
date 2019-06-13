module Hode.Bughunt.PTree where

import           Data.List.PointedList (PointedList)

-- | `PTree` is a tree made of `PointedList`s
data PTree a = PTree {
  _pTreeLabel :: a
  , _pMTrees :: Maybe (Porest a) }
  deriving (Eq, Show)
type Porest a =  PointedList (PTree a)
