{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

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

unEitherEltVar :: Subst -> Either Elt Var -> (Maybe Elt, Maybe Var)
unEitherEltVar _ (Left e) = (Just e, Nothing)
unEitherEltVar s (Right v) = (M.lookup v s, Just v)


-- | = `Test`s

isNot_1 :: Either Elt Var -> Test
isNot_1 eev = Test go deps where
  go :: Data -> Subst -> Elt -> Bool
  go _ s = let (me, mv) = unEitherEltVar s eev :: (Maybe Elt, Maybe Var)
               err = error $ keyErr "isNot_1" (fromJust mv) s
           in maybe err (/=) me
  deps :: Set Var
  deps = S.fromList $ catMaybes $ map (either (const Nothing) Just) [eev]


-- | = `VarTest`s

isNot_2 :: Either Elt Var -> Either Elt Var -> VarTest
isNot_2 eev eev' = VarTest go deps where
  go :: Data -> Subst -> Bool
  go _ s = let (me , mv ) = unEitherEltVar s eev  :: (Maybe Elt, Maybe Var)
               (me', mv') = unEitherEltVar s eev' :: (Maybe Elt, Maybe Var)
               err  = error $ keyErr "isNot_2" (fromJust mv ) s
               err' = error $ keyErr "isNot_2" (fromJust mv') s
    in case me of Nothing -> err
                  Just _ -> case me' of Nothing -> err'
                                        Just _ -> me == me'
  deps :: Set Var
  deps = S.fromList $ catMaybes
    $ map (either (const Nothing) Just) [eev,eev']


-- | = `Find`s

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
