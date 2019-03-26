{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Util.PTree where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as P
import           Data.Maybe
import           Data.Functor.Foldable.TH

import Util.Direction


instance Ord a => Ord (PointedList a) where
  compare pl ql = compare (toList pl) (toList ql)

data PTree a = PTree { _pTreeLabel :: a
                     , _pTreeHasFocus :: Bool -- ^ PITFALL: permits invalid
  -- state. There should be only one focused node anywhere in the tree.
                     , _pMTrees :: Maybe (Porest a)
                     }
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Porest a =  PointedList (PTree a)


-- | = Lenses

makeLenses      ''PTree
makeBaseFunctor ''PTree
makeLenses      ''PTreeF

-- TODO : These lenses are inefficient, because they convert a `PointedList`
-- to a normal list in order to find the focused element. If a subtree
-- has focus, its parent should be focused on it. And note that
-- there is already a nice `Functor` instance for `PointedList`.

getFocusedChild :: Getter (PTree a) (Maybe (PTree a))
getFocusedChild = to go where
  go :: PTree a -> Maybe (PTree a)
  go (_pTreeHasFocus -> True) = Nothing
  go t = case _pMTrees t of
    Nothing -> Nothing
    Just ts -> listToMaybe $ filter _pTreeHasFocus $ toList ts

getParentOfFocusedSubtree :: Getter (PTree a) (Maybe (PTree a))
getParentOfFocusedSubtree = to go where
  go :: PTree a -> Maybe (PTree a)
  go t = if isJust $ t ^. getFocusedChild then Just t else
    case t ^. pMTrees of
      Nothing -> Nothing
      Just ts -> listToMaybe $ map fromJust
                 $ filter isJust $ map go $ toList ts

setParentOfFocusedSubtree :: Setter' (PTree a) (PTree a)
setParentOfFocusedSubtree = sets go where
  go :: (PTree a -> PTree a) -> PTree a -> PTree a
  go f t = if isJust $ t ^. getFocusedChild
    then f t else
    case t ^. pMTrees of
      Nothing -> t
      Just ts -> let ts' = map (go f) $ toList ts
                 in t & pMTrees .~ P.fromList ts'

getFocusedSubtree :: Getter (PTree a) (Maybe (PTree a))
getFocusedSubtree = to go where
  go :: PTree a -> Maybe (PTree a)
  go t@(_pTreeHasFocus -> True) = Just t
  go t = case _pMTrees t of
    Nothing -> Nothing
    Just ts -> listToMaybe $ map fromJust $ filter isJust $
               map go $ toList ts

setFocusedSubtree :: Setter' (PTree a) (PTree a)
setFocusedSubtree = sets go where
  go :: forall a. (PTree a -> PTree a) -> PTree a -> PTree a
  go f t@(_pTreeHasFocus -> True) = f t
  go f t = case _pMTrees t of
    Nothing -> t
    Just pts -> let (tsRec :: [PTree a]) = map (go f) $ toList pts
                in t & pMTrees .~ P.fromList tsRec


-- | = Creators

pTrees :: Traversal' (PTree a) (Porest a)
pTrees = pMTrees . _Just

pTreeLeaf :: a -> PTree a
pTreeLeaf a = PTree { _pTreeLabel = a
                    , _pTreeHasFocus = False
                    , _pMTrees = Nothing }

porestLeaf :: a -> Porest a
porestLeaf = P.singleton . pTreeLeaf


-- | = Modifiers

consUnderAndFocus :: forall a. PTree a -> PTree a -> PTree a
consUnderAndFocus newMember host =
  let (ts' :: [PTree a]) = case _pMTrees host of
                             Nothing -> m : []
                             Just ts -> m : toList ts
        where m = newMember & pTreeHasFocus .~ True
  in host & pTreeHasFocus .~ False
          & pMTrees .~ P.fromList ts'

--moveFocusInTree :: Direction -> PTree a -> PTree a
--moveFocusInTree Up
