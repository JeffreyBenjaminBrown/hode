{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Util.UTypes where

import Data.Functor.Foldable.TH
import Data.Vector (Vector)
import Control.Lens.TH


data Direction = DirPrev | DirNext | DirUp | DirDown
  deriving (Show,Eq, Ord)

-- | A `Path` through a `VTree`. The head of the list is where
-- to go from the `VTree`'s root to its next layer.
type Path = [Int]

-- | A `Vath` is a path through a nonempty `Vorest`.
type Vath = (Int, Path)

-- | A vector-based tree, for viewing things like Views, Buffers, ...
data VTree a = VTree {
    _vTreeLabel :: a
  , _vTrees :: Vorest a
  , _vTreeFocus :: Int -- ^ meaningless if `viewSubviews` empty
  , _vTreeIsFocused :: Bool } deriving (Eq, Show, Ord, Functor)
type Vorest a = Vector (VTree a)

makeLenses      ''VTree
makeBaseFunctor ''VTree
makeLenses      ''VTreeF
