{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Util.PTree where

import           Control.Lens.TH
import           Data.List.PointedList
import           Data.Functor.Foldable.TH


toList :: PointedList a -> [a]
toList pl = reverse (_reversedPrefix pl) ++
            [_focus pl] ++ _suffix pl

instance Ord a => Ord (PointedList a) where
  compare pl ql = compare (toList pl) (toList ql)

data PTree a = PTree { _pTreeLabel :: a
                     , _pTrees :: Maybe (Porest a) }
  -- PITFALL: Rather than include a "focused" field, just change the
  -- label type when focus (or other info) is needed.
  deriving (Eq, Show, Functor)
type Porest a =  PointedList (PTree a)

makeLenses      ''PTree
makeBaseFunctor ''PTree
makeLenses      ''PTreeF

pTreeLeaf :: a -> PTree a
pTreeLeaf a = PTree { _pTreeLabel = a
                    , _pTrees = Nothing }
