{-# LANGUAGE OverloadedStrings #-}

module Hode.Brick.Help where

import Control.Lens
import qualified Data.List                      as L
import qualified Data.List.PointedList          as P
import qualified Data.List.PointedList.Circular as C
import qualified Graphics.Vty                   as V

import qualified Brick.Widgets.Center       as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Border       as B
import qualified Brick.AttrMap              as B
import qualified Brick.Main                 as B
import qualified Brick.Types                as B
import qualified Brick.Util                 as B
import qualified Brick.Widgets.Core         as B

import Hode.Brick.Help.FakeData
import Hode.Brick.Help.Types


isFocusedWindow :: Help -> WindowName -> Bool
isFocusedWindow st = (==) (st ^. helpWindows . P.focus)

windowFont :: Help -> WindowName -> B.AttrName
windowFont st wn = if wn == st ^. helpWindows . P.focus
                   then "focus"
                   else "no focus"

initState :: Choice1Plist -> Help
initState cs = Help
  { _helpWindows = allWindowNammes
  , _helpChoices = cs
  , _helpHelp = False }

pListToList_withFocus :: (a -> b) -> (a -> b) -> P.PointedList a -> [b]
pListToList_withFocus normal focus as =
  map normal (as ^. P.reversedPrefix . reversed) ++
  [focus     (as ^. P.focus)] ++
  map normal (as ^. P.suffix)

ui :: Help -> B.Widget WindowName
ui st = let
  padding = 1
  normal, highlight :: B.AttrName -> String -> B.Widget n
  normal    stylePrefix s =
    B.padLeftRight padding .
    B.withAttr stylePrefix .                  B.str $ s
  highlight stylePrefix s =
    B.padLeftRight padding . B.visible .
    B.withAttr (stylePrefix <> "highlight") . B.str $ s
  drawPList :: B.AttrName -> C.PointedList String -> [B.Widget n]
  drawPList sp = pListToList_withFocus (normal sp) (highlight sp)

  in
  B.vBox
  [ B.strWrap ( if st ^. helpHelp
                then concat $ L.intersperse "\n"
                   [ "Press Esc to quit. Press h to hide this message."
                   , "Press the spacebar to switch menus."
                   , "Press d and e to scroll between options."
                   , "The mode you choose determines which submodes are possible. The submode you chooses determines which commands are possible. The command you choose will be described in the big window. If it doesn't all fit on one screen, navigate to the big window and scroll with d and e." ]
                else "For help using this help, press h." )
  , B.hBorder

  , B.vLimit 2 $ B.hBox
    [ B.vBox [ B.str "choose a mode: "
             , B.str "choose a submode: " ]
    , B.withBorderStyle B.ascii B.vBorder
    , B.vBox
      [ B.vLimit 1 $ B.center
        $ B.viewport Choice1 B.Horizontal
        $ B.hBox $ drawPList (windowFont st Choice1)
        $ fmap fst $ st ^. helpChoices
      , B.vLimit 1 $ B.center
        $ B.viewport Choice2 B.Horizontal
        $ B.hBox $ drawPList (windowFont st Choice2)
        $ fmap fst $ st ^. helpChoices2 ] ]

  , B.hBorder
  , B.hBox
    [ B.str "choose a command: "
      B.<=> let cs3 = fmap fst $ st ^. helpChoices3
                limit = -- the stricter limit applies
                  B.hLimitPercent 40
                  . B.hLimit ( (+ (2 * padding))
                               $ maximum $ fmap length cs3)
            in limit $ B.viewport Choice3 B.Vertical $ B.vBox
               $ drawPList (windowFont st Choice3) cs3
    , B.vBorder
    , normal (windowFont st Content)
      $ st ^. helpChoices3 . P.focus . _2 ] ]

theMap :: B.AttrMap
theMap = B.attrMap
         (V.white `B.on` V.black) -- default
         [ ("no focus",                V.white `B.on` V.black)
         , ("no focus" <> "highlight", V.black `B.on` V.green) 
         , ("focus",                   V.white `B.on` V.blue)
         , ("focus" <> "highlight",    V.black `B.on` V.green) ]

respond :: Help -> B.BrickEvent WindowName e
                -> B.EventM WindowName (B.Next Help)
respond st (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> B.halt st
    V.EvKey (V.KChar ' ') [] ->
      B.continue $ st & helpWindows %~ C.next
    V.EvKey (V.KChar 'h') [] ->
      B.continue $ st & helpHelp %~ not
    _ -> case st ^. helpWindows . P.focus of

      Choice1 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & helpChoices %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & helpChoices %~ C.next
        _ -> B.continue st

      Choice2 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & helpChoices2 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & helpChoices2 %~ C.next
        _ -> B.continue st

      Choice3 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & helpChoices3 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & helpChoices3 %~ C.next
        _ -> B.continue st
      _ -> B.continue st
respond st _ = B.continue st

app :: B.App Help e WindowName
app = B.App
      { B.appDraw = (:[]) . ui
      , B.appHandleEvent = respond
      , B.appStartEvent = return
      , B.appAttrMap = const theMap
      , B.appChooseCursor = B.neverShowCursor
      }

help :: Choice1Plist -> IO Help
help = B.defaultMain app . initState

silly :: IO Help
silly = help sillyChoices
