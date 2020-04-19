{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
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

import Hode.Brick.Help.Data
import Hode.Brick.Help.Types


isFocusedWindow :: St -> WindowName -> Bool
isFocusedWindow st = (==) (st ^. windows . P.focus)

windowFont :: St -> WindowName -> B.AttrName
windowFont st wn = if wn == st ^. windows . P.focus
                   then "focus"
                   else "no focus"

initState :: Choice1Plist -> St
initState cs = St
  { _windows = allWindowNammes
  , _choices = cs
  , _helpHelp = False }

pListToList_withFocus :: (a -> b) -> (a -> b) -> P.PointedList a -> [b]
pListToList_withFocus normal focus as =
  map normal (as ^. P.reversedPrefix . reversed) ++
  [focus     (as ^. P.focus)] ++
  map normal (as ^. P.suffix)

ui :: St -> B.Widget WindowName
ui st = let
  normal, highlight :: B.AttrName -> String -> B.Widget n
  normal    stylePrefix s =
    B.padLeftRight 1 .
    B.withAttr stylePrefix .                  B.str $ s
  highlight stylePrefix s =
    B.padLeftRight 1 . B.visible .
    B.withAttr (stylePrefix <> "highlight") . B.str $ s
  drawPList :: B.AttrName -> C.PointedList String -> [B.Widget n]
  drawPList sp = pListToList_withFocus (normal sp) (highlight sp)

  in
  B.vBox
  [ B.strWrap $ if st ^. helpHelp then "Press d and e to scroll between options, space to switch menus, Esc to quit. The mode you choose determines which submodes are possible. The submode you chooses determines which commands are possible. The command you choose will be described in the big window. If it doesn't all fit on one screen, navigate to the big window and scroll with d and e. Press h to hide this message." else "For help using this help, press h."
  , B.hBorder

  , B.vLimit 2 $ B.hBox
    [ B.vBox [ B.str "choose a mode: "
             , B.str "choose a submode: " ]
    , B.withBorderStyle B.ascii B.vBorder
    , B.vBox
      [ B.vLimit 1 $ B.center
        $ B.viewport Choice1 B.Horizontal
        $ B.hBox $ drawPList (windowFont st Choice1)
        $ fmap fst $ st ^. choices
      , B.vLimit 1 $ B.center
        $ B.viewport Choice2 B.Horizontal
        $ B.hBox $ drawPList (windowFont st Choice2)
        $ fmap fst $ st ^. choices2 ] ]

  , B.hBorder
  , B.hBox
    [ B.str "choose a command: "
      B.<=> let cs3 = fmap fst $ st ^. choices3
                limit = -- the stricter limit applies
                  B.hLimitPercent 40
                  . B.hLimit ((+2) $ maximum $ fmap length cs3)
            in limit $ B.viewport Choice3 B.Vertical $ B.vBox
               $ drawPList (windowFont st Choice3) cs3
    , B.vBorder
    , normal (windowFont st Content)
      $ st ^. choices3 . P.focus . _2 ] ]

theMap :: B.AttrMap
theMap = B.attrMap
         (V.white `B.on` V.black) -- default
         [ ("no focus",                V.white `B.on` V.black)
         , ("no focus" <> "highlight", V.black `B.on` V.green) 
         , ("focus",                   V.white `B.on` V.blue)
         , ("focus" <> "highlight",    V.black `B.on` V.green) ]

respond :: St -> B.BrickEvent WindowName e
              -> B.EventM WindowName (B.Next St)
respond st (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> B.halt st
    V.EvKey (V.KChar ' ') [] ->
      B.continue $ st & windows %~ C.next
    V.EvKey (V.KChar 'h') [] ->
      B.continue $ st & helpHelp %~ not
    _ -> case st ^. windows . P.focus of

      Choice1 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & choices %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & choices %~ C.next
        _ -> B.continue st

      Choice2 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & choices2 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & choices2 %~ C.next
        _ -> B.continue st

      Choice3 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & choices3 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & choices3 %~ C.next
        _ -> B.continue st
      _ -> B.continue st
respond st _ = B.continue st

app :: B.App St e WindowName
app = B.App
      { B.appDraw = (:[]) . ui
      , B.appHandleEvent = respond
      , B.appStartEvent = return
      , B.appAttrMap = const theMap
      , B.appChooseCursor = B.neverShowCursor
      }

help :: Choice1Plist -> IO St
help = B.defaultMain app . initState

silly :: IO St
silly = help sillyChoices
