{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


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
isNot (Left e) =
  Test t mempty
  where
    t :: Data -> Subst -> Elt -> Bool
    t _ _ = (/=) e
isNot (Right v) = -- TODO Maybe dets should just be S.singleton v?
  Test t $ S.union (S.singleton v) dets
  where
    t :: Data -> Subst -> Elt -> Bool
    t _ s = (/=) $ (M.!) s v
    dets = maybe S.empty snd $ varDets v

findChildren :: Either Elt Var -> Find
findChildren (Left e) =
  Find f mempty
  where
    f :: Graph -> Subst -> Set Elt
    f g _ = children g e
findChildren (Right v) = -- TODO Maybe dets should just be S.singleton v?
  Find f $ S.union (S.singleton v) dets
  where
    f :: Graph -> Subst -> Set Elt
    f g s = children g $ (M.!) s v
    dets = maybe S.empty snd $ varDets v

findParents :: Either Elt Var -> Find
findParents (Left e) =
  Find f mempty
  where
    f :: Graph -> Subst -> Set Elt
    f g _ = parents g e
findParents (Right v) = -- TODO Maybe dets should just be S.singleton v?
  Find f $ S.union (S.singleton v) dets
  where
    f :: Graph -> Subst -> Set Elt
    f g s = (parents g) $ (M.!) s v
    dets = maybe S.empty snd $ varDets v
