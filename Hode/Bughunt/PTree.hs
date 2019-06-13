{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hode.Bughunt.PTree where

import           Control.Lens
import           Data.List.PointedList (PointedList)
import           Data.Functor.Foldable.TH


-- | == `PTree`, a list made of `PointedList`s
  
data PTree a = PTree {
  _pTreeLabel :: a
  , _pMTrees :: Maybe (Porest a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)
type Porest a =  PointedList (PTree a)

makeLenses      ''PTree
makeBaseFunctor ''PTree
makeLenses      ''PTreeF


-- | The root has level 0, its children level 1, etc.
writeLevels :: PTree a -> PTree (Int,a)
writeLevels = f 0 where
  f :: Int -> PTree a -> PTree (Int, a)
  f i pt = pt
    { _pTreeLabel = (i, _pTreeLabel pt)
    , _pMTrees = fmap (fmap $ f $ i+1) $ _pMTrees pt }
      -- The outer fmap gets into the Maybe,
      -- and the inner gets into the PointedList.
