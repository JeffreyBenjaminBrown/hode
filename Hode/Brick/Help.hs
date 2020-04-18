{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.List.PointedList          as P
import Data.List.PointedList.Circular as C
import qualified Graphics.Vty         as V

import qualified Brick.AttrMap      as B
import qualified Brick.Main         as B
import qualified Brick.Types        as B
import qualified Brick.Util         as B
import qualified Brick.Widgets.Core as B


type Name = ()
type Focused = Bool
type St = P.PointedList (String, String)

animals :: P.PointedList (String, String)
animals =
  maybe (error "impopssible") id $
  P.fromList [ ("Apple", "Introduced evil to the world. Tasty.")
             , ( "Bird","Flies, melodious." )
             , ( "Marsupial", "Two womby phases!" )
             , ( "Snail","Slimy, fries up real nice." ) ]

ui :: St -> B.Widget n
ui st =
  B.vBox ( map normal (st ^.. reversedPrefix . reversed . folded . _1)   ++
           [highlight (st ^.  focus .                              _1) ] ++
           map normal (st ^.. suffix .                    folded . _1) )
  B.<+> normal (st ^. focus . _2)
  where 
  normal    = B.withAttr "option"                  . B.str
  highlight = B.withAttr ("option" <> "highlight") . B.str

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
          B.continue $ st & C.previous
        V.EvKey (V.KChar 'd') [] ->
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
main = B.defaultMain app animals
