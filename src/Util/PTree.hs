{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Util.PTree where

import           Control.Lens
--import           Control.Lens.Type
import           Control.Lens.TH
import           Data.Foldable (toList)
import           Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as P
import           Data.Maybe
import           Data.Functor.Foldable.TH


instance Ord a => Ord (PointedList a) where
  compare pl ql = compare (toList pl) (toList ql)

data PTree a = PTree { _pTreeLabel :: a
                     , _pTreeFocused :: Bool
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
                    , _pTreeFocused = False
                    , _pMTrees = Nothing }

porestLeaf :: a -> Porest a
porestLeaf = P.singleton . pTreeLeaf

focusedSubtree :: PTree a -> Maybe (PTree a)
focusedSubtree t@(_pTreeFocused -> True) = Just t
focusedSubtree (_pMTrees -> Just ts) =
  listToMaybe $ map fromJust $ filter isJust $
  map focusedSubtree $ toList ts
focusedSubtree _ = Nothing

consUnderAndFocus :: forall a. PTree a -> PTree a -> PTree a
consUnderAndFocus newMember host =
  let (ts' :: [PTree a]) = case _pMTrees host of
                             Nothing -> m : []
                             Just ts -> m : toList ts
        where m = newMember & pTreeFocused .~ True
  in host & pTreeFocused .~ False
          & pMTrees .~ P.fromList ts'
