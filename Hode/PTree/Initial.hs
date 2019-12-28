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
  , prevIfPossible, nextIfPossible -- ^ PointedList a ->        PointedList a
  , nudgePrev, nudgeNext           -- ^ PointedList a ->        PointedList a
  , filterPList     -- ^ (a -> Bool) -> PointedList a -> Maybe (PointedList a)
  , insertLeft_noFocusChange  -- ^ a -> PointedList a ->        PointedList a

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

  -- | ** PTree modifiers
  , writeLevels        -- ^ PTree a -> PTree (Int,a)
  , cons_topNext       -- ^       a -> Porest a -> Porest a
  , consUnder_andFocus -- ^ PTree a -> PTree a -> PTree a
  , deleteInPorest     -- ^ Porest a -> Maybe (Porest a)
  , deleteInPTree      -- ^ PTree a -> PTree a
  , nudgeFocus_inPTree  -- ^ Direction -> PTree a -> PTree a
  , nudgeFocus_inPorest -- ^ Direction -> Porest a -> Porest a
  , nudgeInPorest       -- ^ Direction -> Porest a -> Porest a
  , nudgeInPTree        -- ^ Direction -> PTree a -> PTree a
  ) where

import           Control.Arrow ((>>>))
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

prevIfPossible, nextIfPossible :: PointedList a -> PointedList a
prevIfPossible l = maybe l id $ P.previous l
nextIfPossible l = maybe l id $ P.next     l

nudgePrev, nudgeNext :: PointedList a -> PointedList a
nudgePrev p@(PointedList []     _ _)      = p
nudgePrev   (PointedList (a:as) f bs)     =
             PointedList as     f (a:bs)
nudgeNext p@(PointedList _      _ [])     = p
nudgeNext   (PointedList as     f (b:bs)) =
             PointedList (b:as) f bs

filterPList :: (a -> Bool) -> PointedList a -> Maybe (PointedList a)
filterPList pred pl0@(PointedList as b cs) = let
  pl1 = map (False,) as ++ [(True,b)] ++ map (False,) cs
  l = filter (pred . snd) $ toList pl1
  i = let x = length $ takeWhile (not . fst) l
      in if x == length pl1 then 0 else x
  in case P.fromList $ map snd l of
       Nothing  -> Nothing
       Just pl1 -> P.moveTo i pl1

insertLeft_noFocusChange :: a -> PointedList a -> PointedList a
insertLeft_noFocusChange a =
  P.tryNext . P.insertLeft a


-- | *** `PTree`, a tree made of `PointedList`s

data PTree a = PTree {
    _pTreeLabel :: a
  , _pTreeHasFocus :: Bool -- ^ PITFALL: permits invalid state.
    -- There should be only one focused node in the tree.
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


-- | ** PTree modifiers

-- | The root has level 0, its children level 1, etc.
writeLevels :: PTree a -> PTree (Int,a)
writeLevels = f 0 where
  f :: Int -> PTree a -> PTree (Int, a)
  f i pt = pt
    { _pTreeLabel = (i, _pTreeLabel pt)
    , _pMTrees = fmap (fmap $ f $ i+1) $ _pMTrees pt }
      -- The outer fmap reaches into the Maybe,
      -- and the inner reaches into the PointedList.

cons_topNext :: a -> Porest a -> Porest a
cons_topNext a =
  P.focus . pTreeHasFocus .~ False >>>
  P.insertRight (pTreeLeaf a)      >>> -- moves focus to `a`
  P.focus . pTreeHasFocus .~ True

-- | Inserts `newFocus` under `oldFocus`, and focuses on the newcomer.
consUnder_andFocus :: forall a. PTree a -> PTree a -> PTree a
consUnder_andFocus newFocus oldFocus =
  let ts'' :: [PTree a] =
        let m = newFocus & pTreeHasFocus .~ True
        in case _pMTrees oldFocus of
             Nothing -> [m]
             Just ts ->
               let ts' = ts & P.focus . pTreeHasFocus .~ False
               in m : toList ts'
  in oldFocus & pTreeHasFocus .~ False
              & pMTrees .~ P.fromList ts''

nudgeFocus_inPTree :: Direction -> PTree a -> PTree a
nudgeFocus_inPTree DirUp t =
  case t ^? getParentOfFocusedSubtree . _Just of
    Nothing -> t & pTreeHasFocus .~ True
    Just st -> let
      st' = st & pMTrees . _Just . P.focus . pTreeHasFocus .~ False
               &                             pTreeHasFocus .~ True
      in t & setParentOfFocusedSubtree .~ st'

nudgeFocus_inPTree DirDown t =
  case t ^? getFocusedSubtree . _Just . pMTrees . _Just
  of Nothing -> t
     Just ts -> let ts' = ts & P.focus . pTreeHasFocus .~ True
                in t & setFocusedSubtree . pMTrees .~ Just ts'
                     & setFocusedSubtree . pTreeHasFocus .~ False

nudgeFocus_inPTree DirPrev t =
  case t ^? getParentOfFocusedSubtree . _Just . pMTrees . _Just
  of Nothing -> t -- happens at the top of the tree
     Just ts -> let ts' = ts & P.focus . pTreeHasFocus .~ False
                             & prevIfPossible
                             & P.focus . pTreeHasFocus .~ True
       in t & setParentOfFocusedSubtree . pMTrees .~ Just ts'

nudgeFocus_inPTree DirNext t =
  case t ^? getParentOfFocusedSubtree . _Just . pMTrees . _Just
  of Nothing -> t -- happens at the top of the tree
     Just ts -> let ts' = ts & P.focus . pTreeHasFocus .~ False
                             & nextIfPossible
                             & P.focus . pTreeHasFocus .~ True
       in t & setParentOfFocusedSubtree . pMTrees .~ Just ts'

nudgeFocus_inPorest :: Direction -> Porest a -> Porest a
nudgeFocus_inPorest d p =
  case p ^. P.focus . pTreeHasFocus
  of False -> p & P.focus %~ nudgeFocus_inPTree d
     True -> case d of
       DirUp   -> p -- it's already as `DirUp` as it can be
       DirDown -> p & P.focus %~ nudgeFocus_inPTree d
       DirNext -> p & P.focus . pTreeHasFocus .~ False
                     & nextIfPossible
                     & P.focus . pTreeHasFocus .~ True
       DirPrev -> p & P.focus . pTreeHasFocus .~ False
                     & prevIfPossible
                     & P.focus . pTreeHasFocus .~ True

deleteInPorest :: Porest a -> Maybe (Porest a)
deleteInPorest p =
  case p ^. P.focus . pTreeHasFocus
  of True ->  (P.focus . pTreeHasFocus .~ True)
              <$> P.deleteLeft p
     False -> Just $ p & P.focus %~ deleteInPTree

deleteInPTree :: forall a. PTree a -> PTree a
deleteInPTree t =
  case t ^? ( getParentOfFocusedSubtree . _Just
               . pMTrees . _Just )
  of Nothing              -> t -- you can't delete the root
     Just (p :: Porest a) -> t &
       ( setParentOfFocusedSubtree . pMTrees
         .~ deleteInPorest p )

nudgeInPorest :: forall a. Direction -> Porest a -> Porest a
-- PITFALL: Not tested, but it's exactly the same idiom as
-- `nudgeFocus_inPorest`, which is tested.
nudgeInPorest dir p =
  case p ^. P.focus . pTreeHasFocus
  of False -> p & P.focus %~ nudgeInPTree dir
     True -> case dir of
               DirPrev -> nudgePrev p
               DirNext -> nudgeNext p
               _       -> p -- you can only nudge across the same level

nudgeInPTree :: forall a. Direction -> PTree a -> PTree a
nudgeInPTree dir =
  setParentOfFocusedSubtree . pMTrees . _Just
  %~ nudgeInPorest dir
