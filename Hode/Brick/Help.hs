{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import           Data.Foldable (toList)
import qualified Data.List                      as L
import qualified Data.Map                       as M
import qualified Data.List.PointedList          as P
import qualified Data.List.PointedList.Circular as C
import qualified Graphics.Vty                   as V

import qualified Brick.Widgets.Border as B
import qualified Brick.AttrMap        as B
import qualified Brick.Main           as B
import qualified Brick.Types          as B
import qualified Brick.Util           as B
import qualified Brick.Widgets.Core   as B


type Name = ()
type Focused = Bool
type St = P.PointedList (String, BaseMenu)
type BaseMenu = P.PointedList (String, String)

animals :: P.PointedList (String, String)
animals =
  maybe (error "impopssible") id $
  P.fromList [ ("Apple", "Introduced evil to the world. Tasty.")
             , ( "Bird","Flies, melodious." )
             , ( "Marsupial", "Two womby phases!" )
             , ( "Snail","Slimy, fries up real nice." ) ]

balls :: P.PointedList (String, String)
balls =
  maybe (error "impopssible") id $
  P.fromList [ ("Basketball","Very bouncy.")
             , ( "Softball","Lies! What the hell?" )
             , ( "Tennis ball", "Somehow extra awesome." )
             , ( "Mercury","Usually not in the form of a ball." ) ]

chemicals :: P.PointedList (String, String)
chemicals =
  maybe (error "impopssible") id $
  P.fromList [ ("sugar", "long carbohydrate polymers")
             , ( "DMT", "illegal. Naturally manufactured by the brain." )
             , ( "capsaicin", "Intense. Probably misspelled." )
             , ( "DNA", "Hardest language ever." ) ]

menus :: St
menus = maybe (error "impossible") id $
  P.fromList [ ("animals",   animals)
             , ("balls",     balls)
             , ("chemicals", chemicals) ]

pListToList_withFocus :: (a -> b) -> (a -> b) -> P.PointedList a -> [b]
pListToList_withFocus normal focus as =
  map normal (as ^. P.reversedPrefix . reversed) ++
  [focus     (as ^. P.focus)] ++
  map normal (as ^. P.suffix)

ui :: St -> B.Widget n
ui st = let
  (c :: String, b :: BaseMenu) = st ^. P.focus
  normal    = B.padLeftRight 1 . B.withAttr "option"                  . B.str
  highlight = B.padLeftRight 1 . B.withAttr ("option" <> "highlight") . B.str
  in
  B.hBox [ B.vBox $ pListToList_withFocus normal highlight $ fmap fst b
         , B.vBorder
         , normal $ b^. P.focus . _2 ]
  B.<=> ( B.vLimit 1 $ B.hBox $
          L.intersperse B.vBorder $
          pListToList_withFocus normal highlight $ fmap fst st )

theMap :: B.AttrMap
theMap = B.attrMap
         (V.white `B.on` V.black) -- default
         [ ("option",                  V.white `B.on` V.black)
         , ("option" <> "highlight", V.white `B.on` V.blue) ]

respond :: St -> B.BrickEvent Name e -> B.EventM Name (B.Next St)
respond st (B.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> B.halt st
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & P.focus . _2 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & P.focus . _2 %~ C.next
        V.EvKey (V.KChar 's') [] ->
          B.continue $ st & C.previous
        V.EvKey (V.KChar 'f') [] ->
          B.continue $ st & C.next
        _ -> B.continue st
respond st _ = B.continue st

app :: B.App St e ()
app = B.App
      { B.appDraw = (:[]) . ui
      , B.appHandleEvent = respond
      , B.appStartEvent = return
      , B.appAttrMap = const theMap
      , B.appChooseCursor = B.neverShowCursor
      }

main :: IO St
main = B.defaultMain app menus
