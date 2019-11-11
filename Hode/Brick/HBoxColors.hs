-- | Based on one of the tutorial programs from Brick.
-- Probably the one called Attr-something.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Hode.Brick.HBoxColors where

import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black, withURL
  )

import Brick.Main
import Brick.Types (Widget)
import Brick.Widgets.Core
  ( (<=>)
  , withAttr
  , vBox, hBox
  , str
  , hyperlink
  , modifyDefAttr
  )
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)
import qualified Graphics.Vty         as V

import Hode.Brick

main :: IO ()
main = flip defaultMain () $
  ( App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        } :: App () e () )

globalDefault :: Attr
globalDefault = white `on` blue

theMap :: AttrMap
theMap = let
  rc :: Int -> Int -> Int -> V.Color
    = V.rgbColor
  in attrMap globalDefault
    [ ( "sepColor"         , rc 255 255 255 `on` rc 1 0 0)
    , ( "textColor"        , rc 255 255 255 `on` rc 0 1 0)
    , ( "addrColor"        , rc 255 255 255 `on` rc 0 0 1)
    ]

ui :: Widget n
ui = vBox [
    str "Specifying an attribute name bla bla bla."
  , hBox [ withAttr (colorToAttrName TextColor) $ str "Hello, ",
           withAttr (colorToAttrName AddrColor) $ str "Dolly!" ]
  , attrStringWrap' 10
      [ ("12345", TextColor)
      , ("123 45", AddrColor)
      , ("123 45", TextColor)
      , ("123 45", SepColor)
      , ("12345", SepColor)
      ]
  ]
