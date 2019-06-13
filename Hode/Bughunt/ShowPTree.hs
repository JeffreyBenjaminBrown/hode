{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Bughunt.ShowPTree where

import           Data.Foldable (toList)

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.Bughunt.PTree


porestToWidget' :: forall a b n.
     (b -> B.Widget n)
  -> (a -> b) -- ^ shows the columns corresponding to each node
  -> (a -> b) -- ^ shows the nodes that will be arranged like a tree
  -> Porest a -- ^ The Porest to show
  -> B.Widget n
porestToWidget' b2w showColumns showNodes = fShow where

  fShow :: Porest a -> B.Widget n
  fShow = vBox . map recursiveWidget . toList

  oneTreeRowWidget :: PTree a -> B.Widget n
  oneTreeRowWidget t =
    let a :: a = _pTreeLabel t
    in hBox
       [ b2w $ showColumns a
       , b2w $ showNodes $ _pTreeLabel t ]

  recursiveWidget :: PTree a -> B.Widget n
  recursiveWidget pt =
    oneTreeRowWidget pt <=> rest where
    rest = case _pMTrees pt of
             Nothing -> emptyWidget
             Just pts -> fShow pts

listToWidget' :: forall a b n.
     (b -> B.Widget n)
  -> (a -> b) -- ^ shows the columns corresponding to each node
  -> (a -> b) -- ^ shows the nodes that will be arranged like a tree
  -> [a] -- ^ The Porest to show
  -> B.Widget n
listToWidget' b2w showColumns showNodes = recursiveWidget where

  oneTreeRowWidget :: a -> B.Widget n
  oneTreeRowWidget a = hBox
                       [ b2w $ showColumns a
                       , b2w $ showNodes a ]

  recursiveWidget :: [a] -> B.Widget n
  recursiveWidget [] = emptyWidget
  recursiveWidget (a:as) =
    oneTreeRowWidget a <=> rest where
    rest = case as of
             [] -> emptyWidget
             pts -> recursiveWidget as
