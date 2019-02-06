{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import SeekSeq.Query.MkLeaf
import SeekSeq.STypes
import Util


data Graph e = Graph {
    graphNodes    :: Set e             -- ^ good for disconnected graphs
  , graphChildren :: Map e (Set e)   -- ^ keys are parents
  , graphParents  :: Map e (Set e) } -- ^ keys are children
  deriving (Show, Eq, Ord)


-- | Building and reading graphs

-- | Given a list of (parent, [child]) pairs, this produces a graph.
-- It can only create graphs with no isolated nodes.
graph :: Ord e => [( e, [e] )] -> Graph e
graph pairs = Graph nodes children $ invertMapToSet children where
  children = M.fromList $ map f pairs
    where f (a,b) = (a, S.fromList b)
  nodes = S.union (M.keysSet children) $ M.foldl S.union S.empty children

parents :: Ord e => Graph e -> e -> Set e
parents g i  = maybe mempty id $ M.lookup i $ graphParents g
children :: Ord e => Graph e -> e -> Set e
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


-- | == building `Query`s to search a `Graph`

findChildren, findParents :: (Ord e, Show e)
                          => Either e Var -> Find e (Graph e)
findChildren = findFrom "findChildren"
  $ \g e -> Right $ children g e
findParents  = findFrom "findParents"
  $ \g e -> Right $ parents g e
