{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
, ScopedTypeVariables
, TemplateHaskell
, TupleSections
, TypeFamilies
, ViewPatterns #-}

module Hode.PTree.Initial (
    Direction(..)

  -- | *** `PointedList`
  , PointedList(..) -- ^ exports the Ord instance
  , getPList                       -- ^ Getter  (PointedList a) [a]
  , setPList                       -- ^ Setter' (PointedList a) [a]

  -- | *** `PTree`, a tree made of `PointedList`s
  , PTree(..)
  , Porest
  , pMTrees
  , pTreeLabel
  , pTreeHasFocus -- ^ PITFALL: permits invalid state.
  , pMTreesF
  , pTreeLabelF
  , pTreeHasFocusF

  -- | ** PTree optics
  , getFocusedChild           -- ^ Getter  (PTree a) (Maybe (PTree a))
  , getFocusedSubtree         -- ^ Getter  (PTree a) (Maybe (PTree a))
  , setFocusedSubtree         -- ^ Setter' (PTree a) (PTree a)
  , getParentOfFocusedSubtree -- ^ Getter  (PTree a) (Maybe (PTree a))
  , setParentOfFocusedSubtree -- ^ Setter' (PTree a) (PTree a)
  , pTrees                    -- ^ Traversal' (PTree a) (Porest a)

  -- | ** PTree creators
  , pTreeLeaf          -- ^ a -> PTree a
  , porestLeaf         -- ^ a -> Porest a
  ) where

import Prelude hiding (pred)

import           Control.Lens
import           Data.Foldable (toList)
import           Data.List.PointedList (PointedList(..))
import qualified Data.List.PointedList as P
import           Data.Maybe
import           Data.Functor.Foldable.TH


data Direction = DirPrev | DirNext | DirUp | DirDown
  deriving (Show,Eq, Ord)

-- | *** `PointedList`

instance Ord a => Ord (PointedList a) where
  compare pl ql = compare (toList pl) (toList ql)

getPList :: Getter (PointedList a) [a]
getPList = to toList

setPList :: Setter' (PointedList a) [a]
setPList = sets go where
  go :: ([a] -> [a]) -> PointedList a -> PointedList a
  go f pl = case f $ toList pl of
              [] -> pl
              x -> maybe (error msg) id $ P.fromList x
    where msg = "setList: Impossible: x is non-null, so P.fromList works"


-- | *** `PTree`, a tree made of `PointedList`s

data PTree a = PTree {
    _pTreeLabel :: a
  , _pTreeHasFocus :: Bool -- ^ PITFALL: permits invalid state.
    -- There should be exactly one focused node in any complete* tree
    -- (* subtrees can reasonably have zero focused nodes).
    -- PITFALL: The entire path to the focus is marked,
    -- not via this field, but via the focus of each Porest.
  , _pMTrees :: Maybe (Porest a) }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)
type Porest a = PointedList (PTree a)
  -- ^ PITFALL: Folding over a Porest is a little confusing.
  -- See Hode.Test.TPTree.


-- | ** PTree optics

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
    -- If it has focus, none of its children should.
  go t = case _pMTrees t of
    Nothing -> Nothing
    Just ts -> listToMaybe $ filter _pTreeHasFocus $ toList ts
      -- Since at most one child should have focus,
      -- listToMaybe encounters a list with either 0 or 1 elements.

-- | If the `PTree` has more than one subtree for which
-- `pTreeHasFocus` is true (which it shouldn't), this returns the first.
getFocusedSubtree :: Getter (PTree a) (Maybe (PTree a))
getFocusedSubtree = to go where
  go :: PTree a -> Maybe (PTree a)
  go t@(_pTreeHasFocus -> True) = Just t
  go t = case _pMTrees t of
    Nothing -> Nothing
    Just ts -> go $ ts ^. P.focus

-- | Change something about the focused subtree.
setFocusedSubtree :: Setter' (PTree a) (PTree a)
setFocusedSubtree = sets go where
  go :: forall a. (PTree a -> PTree a) -> PTree a -> PTree a
  go f t@(_pTreeHasFocus -> True) = f t
  go f t = case _pMTrees t of
    Nothing -> t
    Just _ -> t & pMTrees . _Just . P.focus %~ go f

getParentOfFocusedSubtree :: Getter (PTree a) (Maybe (PTree a))
getParentOfFocusedSubtree = to go where
  go :: PTree a -> Maybe (PTree a)
  go t = if t ^. pTreeHasFocus            then Nothing
    else if isJust $ t ^. getFocusedChild then Just t
    else case t ^. pMTrees of
           Nothing -> Nothing
           Just ts -> go $ ts ^. P.focus

setParentOfFocusedSubtree :: Setter' (PTree a) (PTree a)
setParentOfFocusedSubtree = sets go where
  go :: (PTree a -> PTree a) -> PTree a -> PTree a
  go f t = if t ^. pTreeHasFocus          then t
    else if isJust $ t ^. getFocusedChild then f t else
    case t ^. pMTrees of
      Nothing -> t
      Just _ -> t & pMTrees . _Just . P.focus %~ go f

pTrees :: Traversal' (PTree a) (Porest a)
pTrees = pMTrees . _Just


-- | ** PTree creators

pTreeLeaf :: a -> PTree a
pTreeLeaf a = PTree { _pTreeLabel = a
                    , _pTreeHasFocus = False
                    , _pMTrees = Nothing }

porestLeaf :: a -> Porest a
porestLeaf = P.singleton . pTreeLeaf
