{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Util.VTree (
  Direction(..)
  , Path
  , Vath
  , VTree(..), vTreeLabel, vTrees, vTreeFocus, vTreeIsFocused
  , Vorest
  , VTreeF(..), vTreeLabelF, vTreesF, vTreeFocusF, vTreeIsFocusedF
  , pathInBounds   -- ^ VTree a -> Path -> Either String ()
  , atPath         -- ^ Path -> Traversal' (VTree a) (VTree a)
  , atVath         -- ^ Vath -> Traversal' (Vorest a) (VTree a)
  , consUnderFocus -- ^ Path -> VTree a -> VTree a -> Either String (VTree a)
  , moveFocus      -- ^ Direction -> (Path, VTree a)
                   -- -> Either String (Path, VTree a)
  ) where

import           Control.Lens.Combinators (from)
import           Control.Lens.TH
import           Data.Functor.Foldable.TH
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Lens
import           Lens.Micro

import Util.Misc


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


pathInBounds :: VTree a -> Path -> Either String ()
pathInBounds _ [] = Right ()
pathInBounds vt (p:ps) = let vs = vt ^. vTrees
  in case inBounds vs p of
  True -> pathInBounds (vs V.! p) ps
  False -> Left $ "pathInBounds: " ++ show p ++ "isn't."

atPath :: Path -> Traversal' (VTree a) (VTree a)
atPath [] = lens id $ flip const -- the trivial lens
atPath (p:ps) = vTrees . from vector
                . ix p . atPath ps

atVath :: Vath -> Traversal' (Vorest a) (VTree a)
atVath (i,p) = from vector . ix i . atPath p

consUnderFocus :: Path -> VTree a -> VTree a -> Either String (VTree a)
consUnderFocus p new host = prefixLeft "consAtFocus" $ do
  _ <- pathInBounds host p
  Right $ host & atPath p . vTrees %~ V.cons new

moveFocus :: Direction -> (Path, VTree a)
          -> Either String (Path, VTree a)
moveFocus DirUp p@([],_) = Right p
moveFocus DirUp (p,a)    = Right (f p, a)
  where f = reverse . tail . reverse
moveFocus DirDown (p,a) = prefixLeft "moveFocus" $ do
  foc <- let err = "bad focus " ++ show p
         in maybe (Left err) Right
            $ a ^? atPath p
  if null $ foc ^. vTrees
    then Right (p                       , a)
    else Right (p ++ [foc ^. vTreeFocus], a)

moveFocus DirPrev (p,a) = do
  _ <- pathInBounds a p
  let pathToParent = take (length p - 1) p
      Just parent = -- safe b/c p is in bounds
        a ^? atPath pathToParent
      parFoc = parent ^. vTreeFocus
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in parent."
  let parFoc' = max 0 $ parFoc - 1
  Right ( p & replaceLast' parFoc'
        , a & atPath pathToParent . vTreeFocus .~ parFoc' )

-- TODO : This duplicates the code for DirPrev.
-- Better: factor out the computation of newFocus,
-- as a function of parent and an adjustment function.
moveFocus DirNext (p,a) = do
  _ <- pathInBounds a p
  let pathToParent = take (length p - 1) p
      Just parent = -- safe b/c p is in bounds
        a ^? atPath pathToParent
      parFoc = parent ^. vTreeFocus
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in parent."
  let parFoc' = min (parFoc + 1)
                $ V.length (parent ^. vTrees) - 1
  Right ( p & replaceLast' parFoc'
        , a & atPath pathToParent . vTreeFocus .~ parFoc' )
