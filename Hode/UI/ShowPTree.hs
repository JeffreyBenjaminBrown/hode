{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.ShowPTree where

import           Data.Foldable (toList)
import           Lens.Micro

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.UI.ITypes
import Hode.Util.PTree


porestToWidget :: forall a.
     (a -> String) -- ^ shows the nodes that will be arranged like a tree
  -> (a -> String) -- ^ shows the columns corresponding to each node
  -> (a -> Bool) -- ^ whether to hide a node's children
  -> (PTree a -> B.Widget BrickName -> B.Widget BrickName)
     -- ^ currently used to show the focused node differently
  -> Porest a -- ^ The Porest to show
  -> B.Widget BrickName
porestToWidget showColumns showIndented isFolded style p0 =
  fShow p where

  p :: Porest (Int, a) = fmap writeLevels p0

  fShow :: Porest (Int,a) -> B.Widget BrickName
  fShow = vBox . map recursiveWidget . toList

  oneTreeRowWidget :: PTree (Int,a) -> B.Widget BrickName
  oneTreeRowWidget t0 =
    let a :: a        = _pTreeLabel t
        t :: PTree a  = fmap snd t0
        indent :: Int = fst $ _pTreeLabel t0
    in style t $ hBox
       [ str $ showColumns a
       , padLeft (B.Pad $ 2 * indent) $
         strWrap $ showIndented $ _pTreeLabel t ]

  recursiveWidget :: PTree (Int,a) -> B.Widget BrickName
  recursiveWidget pt =
    oneTreeRowWidget pt <=> rest where
    rest = case pt ^. pMTrees of
             Nothing -> emptyWidget
             Just pts ->
               case isFolded $ snd $ _pTreeLabel pt of
               True -> emptyWidget
               False -> fShow pts
