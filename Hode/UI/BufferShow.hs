{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Hode.UI.BufferShow (
    resultWindow' -- ^ Buffer -> B.Widget BrickName
  , resultWindow  -- ^ Buffer -> B.Widget BrickName
  , bufferWindow' -- ^ Maybe (Porest Buffer) -> B.Widget BrickName
  ) where

import qualified Data.Map             as M
import           Lens.Micro

import qualified Brick.Types          as B
import           Brick.Widgets.Core
import qualified Brick.AttrMap        as B

import Hode.Brick
import Hode.Brick.Wrap
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.PTree.PShow
import Hode.PTree.Initial


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

    oneRowWidget :: (Bool, ColorString, ColorString) -> B.Widget BrickName
    oneRowWidget (isFocused,cols,node) =
      (if isFocused then visible else id)
      $ hBox
      [ colorStringWrap' 65 (isFocused, cols)
        -- PITFALL: `colorStringWrap` is overkill for `cols`:
        -- `cols` should be too short ever to need wrapping.
        -- Moreover if it does wrap, that means there is no room
        -- for the actual content of the node.
        -- TODO ? write, use a simpler alternative to `colorStringWrap`.
      , colorStringWrap' 65 (isFocused, node) ]

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
    then visible . withAttr (B.attrName "white on green")
    else id

  in maybe (str "There are no results to show (yet).")
     ( viewport (BrickMainName Results) B.Vertical
       . ( porestToWidget (colorStringWrap 65) showColumns
           showNode getFolded focusStyle ) )
     (b ^. bufferRowPorest)

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

    oneRowWidget :: (Bool, ColorString, ColorString) -> B.Widget BrickName
    oneRowWidget (isFocused,cols,node) =
      (if isFocused then visible else id)
      $ hBox
      [ colorStringWrap' 65 (isFocused, cols)
        -- PITFALL: `colorStringWrap` is overkill for `cols`:
        -- `cols` should be empty.
        -- Moreover if it does wrap, that means there is no room
        -- for the actual content of the node.
        -- TODO ? write, use a simpler alternative to `colorStringWrap`.
      , colorStringWrap' 65 (isFocused, node) ]

  in viewport (BrickMainName Results) B.Vertical
     $ vBox $ map oneRowWidget rows
