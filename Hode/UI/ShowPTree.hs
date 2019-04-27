{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.ShowPTree where

import           Data.Foldable (toList)
import           Lens.Micro

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.UI.ITypes
import Hode.Util.PTree


porestToWidget :: forall a
  . (a -> String)
  -> (PTree a -> B.Widget BrickName -> B.Widget BrickName)
    -- ^ currently used to show the focused node differently
  -> Porest a -- ^ The Porest to show
  -> B.Widget BrickName
porestToWidget show0 style p = fShow p where
  fShow :: Porest a -> B.Widget BrickName
  fShow = vBox . map showTreeRec . toList

  showTreeOne, showTreeRec :: PTree a -> B.Widget BrickName
  showTreeOne pt = style pt $ strWrap $ show0 $ _pTreeLabel pt
  showTreeRec pt = showTreeOne pt <=> rest
    where mpts = pt ^. pMTrees
          rest = case mpts of
                   Nothing -> emptyWidget
                   Just pts -> padLeft (B.Pad 2) $ fShow pts
