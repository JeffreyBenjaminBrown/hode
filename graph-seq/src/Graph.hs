{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Util


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

isNot :: Either Elt Var -> Test
isNot (Left e) = Test go mempty
  where
    go :: Data -> Subst -> Elt -> Bool
    go _ _ = (/=) e
isNot (Right v) = Test go $ S.singleton v
  where
    go :: Data -> Subst -> Elt -> Bool
    go _ s = maybe err (/=) $ M.lookup v s
      where err = error $ keyErr "isNot" v s
    dets = maybe S.empty snd $ varDets v

findChildren :: Either Elt Var -> Find
findChildren (Left e) =
  Find go mempty
  where
    go :: Graph -> Subst -> Set Elt
    go g _ = children g e
findChildren (Right v) = Find go $ S.singleton v
  where
    go :: Graph -> Subst -> Set Elt
    go g s = maybe err (children g) $ M.lookup v s
      where err = error $ keyErr "findChildren" v s
    dets = maybe S.empty snd $ varDets v

findParents :: Either Elt Var -> Find
findParents (Left e) = Find go mempty
  where
    go :: Graph -> Subst -> Set Elt
    go g _ = parents g e
findParents (Right v) = Find go $ S.singleton v
  where
    go :: Graph -> Subst -> Set Elt
    go g s = maybe err (parents g) $ M.lookup v s
      where err = error $ keyErr "findParents" v s
    dets = maybe S.empty snd $ varDets v
