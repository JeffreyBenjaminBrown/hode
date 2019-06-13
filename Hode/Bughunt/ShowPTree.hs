{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Bughunt.ShowPTree where

import           Data.Foldable (toList)
import           Lens.Micro

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.Bughunt.PTree


porestToWidget' :: forall a b n.
     (b -> B.Widget n)
  -> (a -> b) -- ^ shows the columns corresponding to each node
  -> (a -> b) -- ^ shows the nodes that will be arranged like a tree
  -> Porest a -- ^ The Porest to show
  -> B.Widget n
porestToWidget' b2w showColumns showNodes p0 =
  fShow p where

  p :: Porest (Int, a) = fmap writeLevels p0

  fShow :: Porest (Int,a) -> B.Widget n
  fShow = vBox . map recursiveWidget . toList

  oneTreeRowWidget :: PTree (Int,a) -> B.Widget n
  oneTreeRowWidget t0 =
    let a :: a        = _pTreeLabel t
        t :: PTree a  = fmap snd t0
    in hBox
       [ b2w $ showColumns a
       , b2w $ showNodes $ _pTreeLabel t ]

  recursiveWidget :: PTree (Int,a) -> B.Widget n
  recursiveWidget pt =
    oneTreeRowWidget pt <=> rest where
    rest = case pt ^. pMTrees of
             Nothing -> emptyWidget
             Just pts -> fShow pts
