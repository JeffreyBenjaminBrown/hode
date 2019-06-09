-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Main where

import qualified Data.Map as M

import qualified Brick.Main           as B
import qualified Brick.Types          as B
import           Brick.Widgets.Core
import qualified Brick.AttrMap        as B
import           Brick.Util (on)
import qualified Graphics.Vty         as B


main :: IO ()
main = B.defaultMain app ()

app :: B.App () e ()
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

appHandleEvent :: () -> B.BrickEvent () e
               -> B.EventM () (B.Next ())
appHandleEvent _ _ = B.halt ()

-- | The focused subview is recalculated at each call to `appDisplay`.
-- Each `ViewExprNodeTree`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: () -> [B.Widget ()]
appDraw () = [x] where
  s = str "can i read this"
  x = vBox [ hBox [ withAttr (B.attrName "b b")  s
                  , withAttr (B.attrName "r b")  s ]
           , hBox [ withAttr (B.attrName "bb b")  s
                  , withAttr (B.attrName "br b")  s ]
           , hBox [ withAttr (B.attrName "b g" )  s
                  , withAttr (B.attrName "r g" )  s ]
           , hBox [ withAttr (B.attrName "bb g")  s
                  , withAttr (B.attrName "br g")  s ]
           , hBox [ withAttr (B.attrName "b gr" )  s
                  , withAttr (B.attrName "r gr" )  s ]
           , hBox [ withAttr (B.attrName "bb gr")  s
                  , withAttr (B.attrName "br gr")  s ]
           ]

onBlack :: String -> B.Widget n -> B.Widget n
onBlack fore = withAttr (B.attrName "blackBack") .
               withAttr (B.attrName fore)
onGray :: String -> B.Widget n -> B.Widget n
onGray fore = withAttr (B.attrName "grayBack") .
               withAttr (B.attrName fore)

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
  [ ( B.attrName "r gr",  B.red        `on` B.rgbColor 0 1 0 )
  , ( B.attrName "b gr",  B.blue       `on` B.rgbColor 0 1 0 )
  , ( B.attrName "br gr", B.brightRed  `on` B.rgbColor 0 1 0 )
  , ( B.attrName "bb gr", B.brightBlue `on` B.rgbColor 0 1 0 )
  , ( B.attrName "r b",  B.red        `on` B.rgbColor 0 0 0 )
  , ( B.attrName "b b",  B.blue       `on` B.rgbColor 0 0 0 )
  , ( B.attrName "br b", B.brightRed  `on` B.rgbColor 0 0 0 )
  , ( B.attrName "bb b", B.brightBlue `on` B.rgbColor 0 0 0 )
  , ( B.attrName "r g",  B.red        `on` B.rgbColor 1 1 1 )
  , ( B.attrName "b g",  B.blue       `on` B.rgbColor 1 1 1 )
  , ( B.attrName "br g", B.brightRed  `on` B.rgbColor 1 1 1 )
  , ( B.attrName "bb g", B.brightBlue `on` B.rgbColor 1 1 1 )
  ]
