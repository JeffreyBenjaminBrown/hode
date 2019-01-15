{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Query
import Types
import Util


-- | Building and reading graphs

graph :: [( Int, [Int] )] -> Graph
graph pairs = Graph nodes children $ invertMapToSet children where
  children = M.fromList $ map f pairs
    where f (a,b) = (a, S.fromList b)
  nodes = S.union (M.keysSet children) $ M.foldl S.union S.empty children

parents :: Graph -> Int -> Set Int
parents g i  = maybe mempty id $ M.lookup i $ graphParents g
children :: Graph -> Int -> Set Int
children g i = maybe mempty id $ M.lookup i $ graphChildren g

invertMapToSet :: forall a. Ord a => Map a (Set a) -> Map a (Set a)
invertMapToSet = foldl addInversion M.empty . M.toList where
  addInversion :: M.Map a ( Set a )
               ->     ( a,  Set a )
               -> M.Map a ( Set a )

  addInversion m (a1, as) -- a1 maps to each a in as
    = S.foldl f m as where
      f :: M.Map  a (S.Set a)
        ->        a
        -> M.Map  a (S.Set a)
      f m a = M.insertWith S.union a (S.singleton a1) m -- each a maps to a1


-- | == for building `Query`s

findChildren = toFind "findChildren" children
findParents  = toFind "findParents"  parents
