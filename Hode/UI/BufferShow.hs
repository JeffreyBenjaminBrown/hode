{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Hode.UI.BufferShow where

import qualified Data.List.PointedList as P
import qualified Data.Map             as M
import           Lens.Micro

import qualified Brick.Main           as B
import qualified Brick.Types          as B
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit   as B
import qualified Brick.AttrMap        as B
import qualified Brick.Focus          as B
import           Brick.Util (on)
import qualified Graphics.Vty         as V

import Hode.Brick
import Hode.Brick.Wrap
import Hode.Rslt.Index (mkRslt)
import Hode.Rslt.RTypes
import Hode.UI.Input
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.IUtil
import Hode.PTree.PShow
import Hode.PTree.Initial


resultWindow' :: Buffer -> B.Widget BrickName
resultWindow' b =
  if null $ b ^. bufferRowPorest
  then (str "There are no results to show (yet).")
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

    style :: Bool -> B.Widget BrickName
                  -> B.Widget BrickName
    style isFocused =
      if isFocused
      then visible . withAttr (B.attrName   "focused result")
      else           withAttr (B.attrName "unfocused result")

    oneRowWidget :: (Bool, ColorString, ColorString) -> B.Widget BrickName
    oneRowWidget (isFocused,cols,node) =
      style isFocused $ hBox
      [ strWrap $ show (isFocused, cols)
      , colorStringWrap 65 cols
        -- PITFALL: `colorStringWrap` is overkill here, because
        -- `cols` should be too short ever to need wrapping.
        -- TODO ? write, use a simpler alternative to `colorStringWrap`.
      , colorStringWrap 65 node]

  in viewport (BrickMainName Results) B.Vertical
     $ vBox $ map oneRowWidget rows

resultWindow :: Buffer -> B.Widget BrickName
resultWindow b = let

  showNode :: BufferRow -> ColorString =
    showColor . _viewExprNode
  getFolded :: BufferRow -> Bool =
    _folded . _otherProps
  showColumns :: BufferRow -> ColorString =
    concatMap ((:[]) . (, TextColor) . show)
    . M.elems . _columnProps

  focusStyle :: PTree a -> B.Widget BrickName
                        -> B.Widget BrickName
  focusStyle bt =
    if bt ^. pTreeHasFocus
    then visible . withAttr (B.attrName   "focused result")
    else           withAttr (B.attrName "unfocused result")

  in maybe (str "There are no results to show (yet).")
     ( viewport (BrickMainName Results) B.Vertical
       . ( porestToWidget (colorStringWrap 65) showColumns
           showNode getFolded focusStyle ) )
     (b ^. bufferRowPorest)
