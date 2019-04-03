-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Main where

import           Data.Foldable (toList)
import qualified Data.List.PointedList as P
import qualified Data.Map             as M
import           Lens.Micro

import qualified Brick.Main           as B
import qualified Brick.Types          as B
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit   as B
import qualified Brick.AttrMap        as B
import qualified Brick.Focus          as B
import           Brick.Util (on)
import qualified Graphics.Vty         as B

import Rslt.Index (mkRslt)
import Rslt.RTypes
import UI.Input
import UI.ITypes
import UI.IUtil
import UI.String
import UI.Window
import Util.PTree


ui :: IO St
ui = uiFromRslt $ mkRslt mempty

uiFromSt :: St -> IO St
uiFromSt = B.defaultMain app

uiFromRslt :: Rslt -> IO St
uiFromRslt = B.defaultMain app . emptySt

app :: B.App St e BrickName
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }


-- | The focused subview is recalculated at each call to `appDisplay`.
-- Each `RsltViewTree`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: St -> [B.Widget BrickName]
appDraw st0 = [w] where
  w = B.center $
    (if st0 ^. showingErrorWindow then errorWindow else mainWindow)
    <=> optionalWindows

  st = st0 & stSetFocusedBuffer .~ b
           & buffers . P.focus . setFocusedSubtree . pTreeHasFocus .~ True
           & stSetFocusedRsltViewTree . pTreeHasFocus .~ True
  (b :: Buffer) = maybe err id $  st0 ^? stGetFocusedBuffer . _Just where
      err = error "Focused Buffer not found."

  mainWindow = case st ^. showingInMainWindow of
    Buffers -> bufferWindow
    CommandHistory -> commandHistoryWindow
    Results -> resultWindow

  optionalWindows =
    ( if (st ^. showingOptionalWindows) M.! Reassurance
      then reassuranceWindow else emptyWidget ) <=>
    ( if (st ^. showingOptionalWindows) M.! Commands
      then commandWindow else emptyWidget )

  commandHistoryWindow, commandWindow, errorWindow, resultWindow, reassuranceWindow, bufferWindow :: B.Widget BrickName

  commandHistoryWindow =
    strWrap $ unlines $ map show $ st0 ^. commandHistory

  commandWindow = vLimit 3
    ( B.withFocusRing (st^.focusRing)
      (B.renderEditor $ str . unlines) (st^.commands) )

  errorWindow = vBox
    [ strWrap $ st ^. uiError
    , padTop (B.Pad 2) $ strWrap $ "(To escape this error message, press Alt-R (to go to Results), Alt-B (Buffers), or Alt-H (command History)." ]

  reassuranceWindow = withAttr (B.attrName "reassurance")
    $ strWrap $ st0 ^. reassurance

  -- TODO: Factor: This duplicates the code for resultWindow.
  -- Also I bet it could be a catamorphism.
  -- (c.f. _hiding/guidance/top-down-catamorphism.hs)
  bufferWindow = viewport (BrickMainName Buffers) B.Vertical
                 $ fShow $ st ^. buffers where
    fShow :: Porest Buffer -> B.Widget BrickName
    fShow = vBox . map showTreeRec . toList

    showTreeOne, showTreeRec :: PTree Buffer -> B.Widget BrickName
    showTreeRec bt = showTreeOne bt <=> rest
      where mpts = bt ^. pMTrees
            rest = case mpts of
                     Nothing -> emptyWidget
                     Just pts -> padLeft (B.Pad 2) $ fShow pts
    showTreeOne bt = style $ strWrap $ _bufferQuery $ _pTreeLabel bt
      where style :: B.Widget BrickName
                  -> B.Widget BrickName
            style = if not $ bt ^. pTreeHasFocus then id
                    else visible
                         . withAttr (B.attrName "focused result")

  resultWindow = viewport (BrickMainName Results) B.Vertical
                 $ showTreeRec $ b ^. bufferRsltViewTree where
    fShow :: Porest RsltView -> B.Widget BrickName
    fShow = vBox . map showTreeRec . toList

    showTreeOne, showTreeRec :: PTree RsltView -> B.Widget BrickName
    showTreeRec bt = showTreeOne bt <=> rest
      where mpts = bt ^. pMTrees
            rest = case mpts of
                     Nothing -> emptyWidget
                     Just pts -> padLeft (B.Pad 2) $ fShow pts
    showTreeOne bt = style $ strWrap $ showRsltView $ _pTreeLabel bt
      where style :: B.Widget BrickName
                  -> B.Widget BrickName
            style = if not $ bt ^. pTreeHasFocus then id
                    else visible
                         . withAttr (B.attrName "focused result")


appChooseCursor :: St -> [B.CursorLocation BrickName]
                -> Maybe (B.CursorLocation BrickName)
appChooseCursor = B.focusRingCursor (^. focusRing)


appHandleEvent :: St -> B.BrickEvent BrickName e
               -> B.EventM BrickName (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc [B.MMeta] -> B.halt st

  -- command window
  B.EvKey (B.KChar 'x') [B.MMeta] -> parseAndRunCommand st
  B.EvKey (B.KChar 'k') [B.MMeta] -> B.continue
    $ emptyCommandWindow st

  -- switch main window content
  B.EvKey (B.KChar 'H') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ CommandHistory
         & showingErrorWindow .~ False
  B.EvKey (B.KChar 'B') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ Buffers
         & showingErrorWindow .~ False
  B.EvKey (B.KChar 'R') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ Results
         & showingErrorWindow .~ False
  -- Brick-focus-related stuff. So far unneeded.
    -- PITFALL: The focused `Window` is distinct from the focused
    -- widget within the `mainWindow`.
    -- B.EvKey (B.KChar '\t') [] -> B.continue $ st & focusRing %~ B.focusNext
    -- B.EvKey B.KBackTab []     -> B.continue $ st & focusRing %~ B.focusPrev

  _ -> case st ^. showingInMainWindow of
    Results -> handleKeyboard_atResultsWindow      st ev
    Buffers -> handleKeyboard_atBufferWindow st ev
    _       -> handleUncaughtInput                  st ev

appHandleEvent st _ = B.continue st


appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr                  , B.white `on` B.blue)
    , (B.editFocusedAttr           , B.black `on` B.yellow)
    , (B.attrName "reassurance"    , B.black `on` B.blue)
    , (B.attrName "focused result" , B.black `on` B.green)
    ]
