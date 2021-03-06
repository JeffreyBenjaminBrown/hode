{-# LANGUAGE OverloadedStrings
, RankNTypes
#-}

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

import Hode.Brick.Help.Types
import Hode.Brick.Help.FakeData


windowFont :: Help -> HelpWindow -> B.AttrName
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

helpUi :: (Ord n, Show n)
  => (HelpWindow -> n) -- ^ `n` is the type used to name windows in the
       -- app into which this help is incorporated.
       -- This argument is probably a constructor of that type.
  -> Help
  -> B.Widget n

helpUi liftName st = let
  padding = 1
  normalWrap, normal, highlight :: B.AttrName -> String -> B.Widget n
  normalWrap stylePrefix s =
    B.padLeftRight padding .
    B.withAttr stylePrefix .                  B.strWrap $ s
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
                   [ "Press Esc to quit the app."
                   , "Press the spacebar to switch menus."
                   , "Press d and e to scroll between options."
                   , concat $ L.intersperse " "
                     [ "The mode you choose determines which submodes are possible."
                     , "The submode you chooses determines which commands are possible."
                     , "The command you choose will be described in the big window."
                     , "If it doesn't all fit on one screen, navigate to the big window and scroll with d and e." ]
                   , "Press h to hide this message."]
                else "For help using this help, press h." )

  , B.hBorder

  , B.vLimit 2 $ B.hBox
    [ B.vBox [ B.str "choose a mode: "
             , B.str "choose a submode: " ]
    , B.withBorderStyle B.ascii B.vBorder
    , B.vBox
      [ B.vLimit 1 $ B.center
        $ B.viewport (liftName Choice1) B.Horizontal
        $ B.hBox $ drawPList (windowFont st Choice1)
        $ fmap fst $ st ^. helpChoices
      , B.vLimit 1 $ B.center
        $ B.viewport (liftName Choice2) B.Horizontal
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
            in limit $ B.viewport (liftName Choice3) B.Vertical
               $ B.vBox $ drawPList (windowFont st Choice3) cs3
    , B.vBorder
    , normalWrap (windowFont st Content)
      $ st ^. helpChoices3 . P.focus . _2 ]
  ]

attrMap :: B.AttrMap
attrMap = B.attrMap
         (V.white `B.on` V.black) -- default
         [ ("no focus",                V.white `B.on` V.black)
         , ("no focus" <> "highlight", V.black `B.on` V.green)
         , ("focus",                   V.white `B.on` V.blue)
         , ("focus" <> "highlight",    V.black `B.on` V.green) ]

respond :: Lens' st Help
        -> (st -> st)
        -> st
        -> B.BrickEvent n e
        -> B.EventM n (B.Next st)
respond help exitHelp st (B.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> B.continue $ exitHelp st
    V.EvKey (V.KChar ' ') [] ->
      B.continue $ st & help . helpWindows %~ C.next
    V.EvKey (V.KChar 'h') [] ->
      B.continue $ st & help . helpHelp %~ not
    _ -> case st ^. help . helpWindows . P.focus of

      Choice1 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & help . helpChoices %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & help . helpChoices %~ C.next
        _ -> B.continue st

      Choice2 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & help . helpChoices2 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & help . helpChoices2 %~ C.next
        _ -> B.continue st

      Choice3 -> case ev of
        V.EvKey (V.KChar 'e') [] ->
          B.continue $ st & help . helpChoices3 %~ C.previous
        V.EvKey (V.KChar 'd') [] ->
          B.continue $ st & help . helpChoices3 %~ C.next
        _ -> B.continue st
      _ -> B.continue st
respond _ _ st _ = B.continue st

-- | This demonstrates what the help menu does.
-- It does not demonstrate how to integrate the help into a bigger app --
-- the definition would need something more complex than
-- `id` and `error` for that.
demo :: forall n. (Ord n, Show n)
  => (HelpWindow -> n) -- ^ `n` is the type used to name windows in the
                       -- app into which this help is incorporated.
                       -- This argument is probably a constructor.
  -> Choice1Plist
  -> IO Help
demo wrapName = let
  app = B.App
        { B.appDraw = (:[]) . helpUi wrapName
        , B.appHandleEvent = respond id $
                             error "this is a goofy way to exit"
        , B.appStartEvent = return
        , B.appAttrMap = const attrMap
        , B.appChooseCursor = B.neverShowCursor
        }
  in B.defaultMain app . initState

silly :: IO Help
silly = demo id sillyChoices
