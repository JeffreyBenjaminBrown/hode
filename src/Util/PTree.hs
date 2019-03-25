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
                     , _pTreeHasFocus :: Bool
                     , _pMTrees :: Maybe (Porest a)
                     }
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Porest a =  PointedList (PTree a)

makeLenses      ''PTree
makeBaseFunctor ''PTree
makeLenses      ''PTreeF

pTrees :: Traversal' (PTree a) (Porest a)
pTrees = pMTrees . _Just

pTreeLeaf :: a -> PTree a
pTreeLeaf a = PTree { _pTreeLabel = a
                    , _pTreeHasFocus = False
                    , _pMTrees = Nothing }

porestLeaf :: a -> Porest a
porestLeaf = P.singleton . pTreeLeaf

focusedChild :: PTree a -> Maybe (PTree a)
focusedChild (_pTreeHasFocus -> True) = Nothing
focusedChild t = case _pMTrees t of
  Nothing -> Nothing
  Just ts -> listToMaybe $ filter _pTreeHasFocus $ toList ts

getFocusedChild :: Getter (PTree a) (Maybe (PTree a))
getFocusedChild = to go where
  go :: PTree a -> Maybe (PTree a)
  go (_pTreeHasFocus -> True) = Nothing
  go t = case _pMTrees t of
    Nothing -> Nothing
    Just ts -> listToMaybe $ filter _pTreeHasFocus $ toList ts

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
    Nothing -> f t
    Just pts -> let
      (ts    :: [PTree a])                     = toList pts
      (tsRec :: [PTree a])                     = map (go f) ts
      (x     :: Maybe (PointedList (PTree a))) = P.fromList tsRec
      in t & pMTrees .~ x

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
