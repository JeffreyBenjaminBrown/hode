{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Hode.UI.BufferShow (
    resultWindow'' -- ^ Buffer -> B.Widget BrickName
  , resultWindow'  -- ^ Buffer -> B.Widget BrickName
  , bufferWindow''  -- ^ Maybe (Porest Buffer) -> B.Widget BrickName
  , bufferWindow'  -- ^ Maybe (Porest Buffer) -> B.Widget BrickName
  ) where

import qualified Data.Map             as M
import           Lens.Micro

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.Brick
import Hode.Brick.Wrap
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.PTree.PShow
import Hode.PTree.Initial


bufferWindow'' :: Maybe (Porest Buffer) -> B.Widget BrickName
bufferWindow'' = let
  name = BrickMainName Results
  showColumns :: Buffer -> [ColorString] =
    const []
  showNode :: Buffer -> ColorString =
    (:[]) . (,TextColor) . _bufferQuery
  getFolded :: Buffer -> Bool =
    const False
  in prestToWidget name showColumns showNode getFolded

resultWindow'' :: Buffer -> B.Widget BrickName
resultWindow'' = let
  name = BrickMainName Results
  showColumns :: BufferRow -> [ColorString] =
    map ((:[]) . (, TextColor) . show)
    . M.elems . _columnProps
  showNode :: BufferRow -> ColorString =
    showColor . _viewExprNode
  getFolded :: BufferRow -> Bool =
    _folded . _otherProps
  in prestToWidget name showColumns showNode getFolded
     . (^. bufferRowPorest)

resultWindow' :: Buffer -> B.Widget BrickName
resultWindow' b =
  if null $ b ^. bufferRowPorest
  then str "There are no results to show (yet)."
  else let
    showColumns :: BufferRow -> [ColorString] =
      map ((:[]) . (, TextColor) . show)
      . M.elems . _columnProps
    showNode :: BufferRow -> ColorString =
      showColor . _viewExprNode
    getFolded :: BufferRow -> Bool =
      _folded . _otherProps

    rows :: [(Bool, ColorString, ColorString)] =
      showPorest' toColorString showColumns showNode getFolded
      ( maybe (error "impossible -- null case already handled")
        id $ b ^. bufferRowPorest )

  in viewport (BrickMainName Results) B.Vertical
     $ vBox $ map oneRowWidget rows

bufferWindow' :: Maybe (Porest Buffer) -> B.Widget BrickName
bufferWindow' p =
  if null p
  then str "There are no buffers to show. Add one with M-S-t."
  else let
    showColumns :: Buffer -> [ColorString] =
      const []
    showNode :: Buffer -> ColorString =
      (:[]) . (,TextColor) . _bufferQuery
    getFolded :: Buffer -> Bool =
      const False
    rows :: [(Bool, ColorString, ColorString)] =
      showPorest' toColorString showColumns showNode getFolded
      $ maybe (error "impossible: null case already handled.")
      id p

  in viewport (BrickMainName Results) B.Vertical
     $ vBox $ map oneRowWidget rows
