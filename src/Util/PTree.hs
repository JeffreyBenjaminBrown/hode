{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Util.PTree where

import           Control.Lens.TH
import           Data.Foldable (toList)
import           Data.List.PointedList (PointedList)
import qualified Data.List.PointedList as P
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
                    , _pTrees = Nothing
                    }

porestLeaf :: a -> Porest a
porestLeaf = P.singleton . pTreeLeaf
