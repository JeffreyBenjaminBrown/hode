{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Util.PTree where

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
                     , _pTrees :: Maybe (Porest a)
                     } deriving (Eq, Show, Functor)
type Porest a =  PointedList (PTree a)

makeLenses      ''PTree
makeBaseFunctor ''PTree
makeLenses      ''PTreeF


pTreeLeaf :: a -> PTree a
pTreeLeaf a = PTree { _pTreeLabel = a
                    , _pTreeFocused = False
                    , _pTrees = Nothing }

porestLeaf :: a -> Porest a
porestLeaf = P.singleton . pTreeLeaf

focusedSubtree :: PTree a -> Maybe (PTree a)
focusedSubtree t@(_pTreeFocused -> True) = Just t
focusedSubtree (_pTrees -> Just ts) =
  head $ filter isJust $ map focusedSubtree $ toList ts
focusedSubtree _ = Nothing
