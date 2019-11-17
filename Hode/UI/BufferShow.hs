{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Hode.UI.BufferShow (
    resultWindow -- ^ Buffer                -> B.Widget BrickName
  , bufferWindow -- ^ Maybe (Porest Buffer) -> B.Widget BrickName
  ) where

import qualified Data.Map             as M
import           Lens.Micro

import qualified Brick.Types          as B

import Hode.Brick
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.PTree.PShow
import Hode.PTree.Initial


bufferWindow :: Maybe (Porest Buffer) -> B.Widget BrickName
bufferWindow = let
  name = BrickMainName Results
  showColumns :: Buffer -> [ColorString] =
    const []
  showNode :: Buffer -> ColorString =
    (:[]) . (,TextColor) . _bufferQuery
  getFolded :: Buffer -> Bool =
    const False
  in porestToWidget name showColumns showNode getFolded

resultWindow :: Buffer -> B.Widget BrickName
resultWindow = let
  name = BrickMainName Results
  showColumns :: BufferRow -> [ColorString] =
    map ((:[]) . (, TextColor) . show)
    . M.elems . _columnProps
  showNode :: BufferRow -> ColorString =
    showColor . _viewExprNode
  getFolded :: BufferRow -> Bool =
    _folded . _otherProps
  in porestToWidget name showColumns showNode getFolded
     . (^. bufferRowPorest)
