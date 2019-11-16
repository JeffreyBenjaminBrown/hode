-- | Based on one of the tutorial programs from Brick.
-- Probably the one called Attr-something.

{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables #-}
module Hode.Brick.HBoxColors (main) where

import Graphics.Vty
import Brick.Main
import Brick.Types (Widget)
import Brick.Widgets.Core
import Brick.Util (on)
import Brick.AttrMap (attrMap, AttrMap)
import qualified Graphics.Vty         as V

import Hode.Brick
import Hode.Brick.Wrap


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
  , hBox [ withAttr (colorToAttrName False TextColor) $ str "Hello, ",
           withAttr (colorToAttrName True  TextColor) $ str "Dolly!" ]
  , colorStringWrap' 10
      ( False, [ ("12345", TextColor)
               , ("123 45", AddrColor)
               , ("123 45", TextColor)
               , ("123 45", SepColor)
               , ("12345", SepColor)
               ] )
  ]
