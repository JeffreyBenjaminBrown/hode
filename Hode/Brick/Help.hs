{-# LANGUAGE OverloadedStrings,
TemplateHaskell #-}

import Control.Lens
--import           Data.Foldable (toList)
import qualified Data.List                      as L
--import qualified Data.Map                       as M
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
type Choice3Plist = P.PointedList (String, String)
type Choice2Plist = P.PointedList (String, Choice3Plist)
type Choice1Plist = P.PointedList (String, Choice2Plist)

-- | Choice 1 determines what is available inn Choice 2,
-- and Choice 2 determines what is available in Choice 3.
-- Collectively, they determine what is in the content window.
data WindowName = Choice1 | Choice2 | Choice3 | Content
  deriving (Show, Eq, Ord)
makeLenses ''WindowName

allWindowNammes :: P.PointedList WindowName
allWindowNammes = maybe (error "impossible") id $
                  P.fromList [ Choice1, Choice2, Choice3, Content ]

data St = St { _choices :: Choice1Plist
             , _windows :: P.PointedList WindowName }
  deriving (Show, Eq)
makeLenses ''St

choices2 :: Lens' St Choice2Plist
choices2 = choices . P.focus . _2

choices3 :: Lens' St Choice3Plist
choices3 = choices . P.focus . _2 . P.focus . _2

isFocusedWindow :: St -> WindowName -> Bool
isFocusedWindow st = (==) (st ^. windows . P.focus)

windowFont :: St -> WindowName -> B.AttrName
windowFont st wn = if wn == st ^. windows . P.focus
                   then "focus"
                   else "no focus"

animals, balls, abab, chemicals, furniture, cfcf :: Choice3Plist
animals =
  maybe (error "impopssible") id $
  P.fromList [ ("Apple", "Introduced evil to the world. Tasty.")
             , ( "Bird","Flies, melodious." )
             , ( "Marsupial", "Two womby phases!" )
             , ( "Snail","Slimy, fries up real nice." ) ]

balls =
  maybe (error "impopssible") id $
  P.fromList [ ("Basketball","Very bouncy.")
             , ( "Mercury","Bigger than a rugby ball, smaller than Saturn." )
             , ( "Softball","Lies! What the hell?" )
             , ( "Tennis ball", "Somehow extra awesome." ) ]

abab =
  maybe (error "impopssible") id $
  P.fromList [ ( "a", "A is for apple." )
             , ( "b", "B is for brownian motion." )
             , ( "c", "C is for Centigrade." )
             , ( "d", "D is for Darwinian." ) ]

chemicals =
  maybe (error "impopssible") id $
  P.fromList [ ("sugar", "long carbohydrate polymers")
             , ( "DMT", "illegal. Naturally manufactured by the brain." )
             , ( "capsaicin", "Intense. Probably misspelled." )
             , ( "DNA", "Hardest language ever." ) ]

furniture =
  maybe (error "impopssible") id $
  P.fromList [ ("chair","Four legs and a butt.")
             , ("Ottoman","A roomy stool.")
             , ("table", "An arrangement of cells into columns and rows.") ]

cfcf =
  maybe (error "impopssible") id $
  P.fromList [ ( "G", "G is for gyroscope." )
             , ( "H", "H is for helium." )
             , ( "I", "I is for Indonesia." )
             , ( "J", "J is for jet skis." ) ]

animals_and_balls, chemicals_and_furniture :: Choice2Plist
animals_and_balls = maybe (error "impossible") id $
                    P.fromList [ ("animals", animals)
                               , ("balls",   balls)
                               , ("abab",    abab)]

chemicals_and_furniture = maybe (error "impossible") id $
                          P.fromList [ ("chemicals", chemicals)
                                     , ("furniture", furniture)
                                     , ("cfcf",      cfcf) ]

initState :: St
initState = St
  { _windows = allWindowNammes
  , _choices = maybe (error "impossible") id $ P.fromList
               [ ("animals_and_balls",       animals_and_balls)
               , ("chemicals_and_furniture", chemicals_and_furniture ) ] }

pListToList_withFocus :: (a -> b) -> (a -> b) -> P.PointedList a -> [b]
pListToList_withFocus normal focus as =
  map normal (as ^. P.reversedPrefix . reversed) ++
  [focus     (as ^. P.focus)] ++
  map normal (as ^. P.suffix)

ui :: St -> B.Widget n
ui st = let
  normal    stylePrefix =
    B.padLeftRight 1 . B.withAttr stylePrefix .                  B.str
  highlight stylePrefix =
    B.padLeftRight 1 . B.withAttr (stylePrefix <> "highlight") . B.str
  drawPList :: B.AttrName -> C.PointedList String -> [B.Widget n]
  drawPList sp = pListToList_withFocus (normal sp) (highlight sp)

  in
  B.<=> ( B.vLimit 1 $ B.hBox $ L.intersperse B.vBorder $
          drawPList (windowFont st Choice1)
          $ fmap fst $ st ^. choices )
  B.<=> ( B.vLimit 1 $ B.hBox $ L.intersperse B.vBorder $
          drawPList (windowFont st Choice2)
          $ fmap fst $ st ^. choices2 )
  B.hBox ( [ B.vBox $ drawPList (windowFont st Choice3)
             $ fmap fst (st ^. choices3)
           , B.vBorder
           , normal (windowFont st Content)
             $  (st ^. choices3) ^. P.focus . _2 ] )

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

main :: IO St
main = B.defaultMain app initState
