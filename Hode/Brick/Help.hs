{-# LANGUAGE OverloadedStrings,
TemplateHaskell #-}

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


type Focused = Bool
type BaseMenu = P.PointedList (String, String)

-- | Choice 1 determines what is available inn Choice 2,
-- and Choice 2 determines what is available in Choice 3.
-- Collectively, they determine what is in the content window.
data WindowName = Choice1 | Choice2 | Choice3 | Content
  deriving (Show, Eq, Ord)
makeLenses ''WindowName
allWindowNammes = maybe (error "impossible") id $
                  P.fromList [ Choice1, Choice2, Choice3, Content ]

data St = St { _choices :: P.PointedList (String, BaseMenu)
             , _windows :: P.PointedList WindowName }
  deriving (Show, Eq)
makeLenses ''St

isFocusedWindow :: St -> WindowName -> Bool
isFocusedWindow st = (==) (st ^. windows . P.focus)

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
             , ( "Mercury","Bigger than a rugby ball, smaller than Saturn." )
             , ( "Softball","Lies! What the hell?" )
             , ( "Tennis ball", "Somehow extra awesome." ) ]

chemicals :: P.PointedList (String, String)
chemicals =
  maybe (error "impopssible") id $
  P.fromList [ ("sugar", "long carbohydrate polymers")
             , ( "DMT", "illegal. Naturally manufactured by the brain." )
             , ( "capsaicin", "Intense. Probably misspelled." )
             , ( "DNA", "Hardest language ever." ) ]

initState :: St
initState = St
  { _windows = allWindowNammes
  , _choices = maybe (error "impossible") id $
               P.fromList [ ("animals",   animals)
                          , ("balls",     balls)
                          , ("chemicals", chemicals) ] }

pListToList_withFocus :: (a -> b) -> (a -> b) -> P.PointedList a -> [b]
pListToList_withFocus normal focus as =
  map normal (as ^. P.reversedPrefix . reversed) ++
  [focus     (as ^. P.focus)] ++
  map normal (as ^. P.suffix)

ui :: St -> B.Widget n
ui st = let
  cs :: P.PointedList (String, BaseMenu) = st ^. choices
  (c :: String, b :: BaseMenu) = cs ^. P.focus
  normal    stylePrefix = B.padLeftRight 1 . B.withAttr style . B.str
    where style = stylePrefix
  highlight stylePrefix = B.padLeftRight 1 . B.withAttr style . B.str
    where style = stylePrefix <> "highlight"
  in
  B.hBox ( let sp = case st ^. windows . P.focus of
                      Choice3 -> "focus"
                      _       -> "no focus"
           in [ B.vBox $ pListToList_withFocus
                (normal sp) (highlight sp) $ fmap fst b
              , B.vBorder
              , normal sp $ b ^. P.focus . _2 ] )
  B.<=> ( let sp = case st ^. windows . P.focus of
                      Choice2 -> "focus"
                      _       -> "no focus"
          in B.vLimit 1 $ B.hBox $ L.intersperse B.vBorder $
             pListToList_withFocus (normal sp) (highlight sp) $
             fmap fst cs )

theMap :: B.AttrMap
theMap = B.attrMap
         (V.white `B.on` V.black) -- default
         [ ("no focus",                V.white `B.on` V.black)
         , ("no focus" <> "highlight", V.white `B.on` V.blue) 
         , ("focus",                   V.black `B.on` V.green)
         , ("focus" <> "highlight",    V.white `B.on` V.blue) ]

respond :: St -> B.BrickEvent WindowName e
              -> B.EventM WindowName (B.Next St)
respond st (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> B.halt st
    V.EvKey (V.KChar ' ') [] ->
      B.continue $ st & windows %~ C.next
    _ -> case st ^. windows . P.focus of
      Choice3 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & choices . P.focus . _2 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & choices . P.focus . _2 %~ C.next
        _ -> B.continue st
      Choice2 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & choices %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & choices %~ C.next
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

main :: IO St
main = B.defaultMain app initState
