{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Hode.UI.BufferShow (
    resultWindow -- ^ Buffer                -> B.Widget BrickName
  , bufferWindow -- ^ Maybe (Porest Buffer) -> B.Widget BrickName
  ) where

import           Control.Lens
import qualified Data.Map             as M

import qualified Brick.Types          as B

import Hode.Brick
import Hode.PTree.Initial
import Hode.PTree.Show
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.Util.Misc


bufferWindow :: Maybe (Porest Buffer) -> B.Widget BrickName
bufferWindow = let
  name = BrickMainName SearchBuffer
  showColumns :: Buffer -> [ColorString] =
    const [] -- there are columns in a results window, not the buffer buffer
  showNode :: Buffer -> ColorString =
    (:[]) . (,TextColor) . showBrief .
    (^. bufferExprRowTree . pTreeLabel . viewExprNode)
  getFolded :: Buffer -> Bool =
    const False
  in porestToWidget name showColumns showNode getFolded

resultWindow :: ViewOptions -> Maybe (Porest ExprRow)
             -> B.Widget BrickName
resultWindow vo = let
  name = BrickMainName SearchBuffer
  showColumns :: ExprRow -> [ColorString]
  showColumns er = let
    bps = _boolProps er
    in [ ( if _selected    bps then "x" else " "
         , if _inSortGroup bps then TextColor else SepColor ) ] :
       ( map ((:[]) . (, TextColor)  . show)
         . M.elems . _numColumnProps
         $ er )
  showNode :: ExprRow -> ColorString =
    showColor vo . _viewExprNode
  getFolded :: ExprRow -> Bool =
    _folded . _otherProps
  in porestToWidget name showColumns showNode getFolded
