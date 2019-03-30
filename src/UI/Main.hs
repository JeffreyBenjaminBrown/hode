-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Main where

import qualified Data.List.PointedList as P
import qualified Data.Map             as M
import qualified Data.Vector          as V
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
import Util.VTree


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

  st = st0 & stBuffer st0                                         .~ b
           & buffers . atVath (st^.vathToBuffer) . vTreeIsFocused .~ True
  (b :: Buffer) = b0 & l .~ True where
    b0 = maybe err id $  st0 ^? stBuffer st where
      err = error "stBuffer returned Nothing in appDraw."
    l = bufferView . atPath (b0 ^. bufferPath) . vTreeIsFocused

  mainWindow = case st ^. showingInMainWindow of
    Buffers -> bufferWindow
    CommandHistory -> commandHistoryWindow
    Results -> resultWindow

  optionalWindows =
    ( if (st ^. showingOptionalWindows) M.! Reassurance
      then reassuranceWindow else emptyWidget ) <=>
    ( if (st ^. showingOptionalWindows) M.! Commands
      then commandWindow else emptyWidget )

  commandHistoryWindow, commandWindow, errorWindow, resultWindow, reassuranceWindow
    :: B.Widget BrickName

  -- TODO: Factor: This duplicates the code for resultWindow.
  bufferWindow = viewport (BrickMainName Buffers) B.Vertical
    $ fShow $ st ^. buffers where

    fShow :: Vorest Buffer -> B.Widget BrickName
    fShow = vBox . map vShowRec . V.toList
    vShowOne, vShowRec :: VTree Buffer -> B.Widget BrickName
    vShowRec bt = vShowOne bt <=>
                  padLeft (B.Pad 2) (fShow $ bt ^. vTrees )
    vShowOne bt = style $ strWrap $ show $ _vTreeLabel bt
      where style :: B.Widget BrickName
                  -> B.Widget BrickName
            style = if not $ bt ^. vTreeIsFocused then id
                    else visible
                         . withAttr (B.attrName "focused result")

  commandHistoryWindow =
    strWrap $ unlines $ map show $ st0 ^. commandHistory

  commandWindow = vLimit 3
    ( B.withFocusRing (st^.focusRing)
      (B.renderEditor $ str . unlines) (st^.commands) )

  errorWindow = vBox
    [ strWrap $ st ^. uiError
    , padTop (B.Pad 2) $ strWrap $ "(To escape this error message, "
      ++ "press Alt-e, Alt-f, Alt-d or Alt-s.)" ]

  resultWindow = viewport (BrickMainName Results) B.Vertical
    $ vShowRec $ b ^. bufferView where

    fShow :: Vorest RsltView -> B.Widget BrickName
    fShow = vBox . map vShowRec . V.toList
    vShowOne, vShowRec :: VTree RsltView -> B.Widget BrickName
    vShowRec vt = vShowOne vt <=>
                  padLeft (B.Pad 2) (fShow $ vt ^. vTrees )
    vShowOne vt = style $ strWrap $ vShow $ _vTreeLabel vt
      where style :: B.Widget BrickName
                  -> B.Widget BrickName
            style = if not $ vt ^. vTreeIsFocused then id
                    else visible
                         . withAttr (B.attrName "focused result")

  reassuranceWindow = withAttr (B.attrName "reassurance") $
    strWrap $ st0 ^. reassurance

appDraw_puffer :: St -> [B.Widget BrickName]
appDraw_puffer st0 = [w] where
  w = B.center $
    (if st0 ^. showingErrorWindow then errorWindow else mainWindow)
    <=> optionalWindows

  st = st0 & stSetFocusedPuffer .~ b
           & puffers . P.focus . setFocusedSubtree . pTreeHasFocus .~ True
           & stSetFocusedRsltViewPTree . pTreeHasFocus .~ True
  (b :: Puffer) = maybe err id $  st0 ^? stGetFocusedPuffer . _Just where
      err = error "Focused Puffer not found."

  mainWindow = case st ^. showingInMainWindow of
    Puffers -> pufferWindow
    CommandHistory -> commandHistoryWindow
    Results -> resultWindow

  optionalWindows =
    ( if (st ^. showingOptionalWindows) M.! Reassurance
      then reassuranceWindow else emptyWidget ) <=>
    ( if (st ^. showingOptionalWindows) M.! Commands
      then commandWindow else emptyWidget )

  commandHistoryWindow, commandWindow, errorWindow, resultWindow, reassuranceWindow, pufferWindow :: B.Widget BrickName

  commandHistoryWindow =
    strWrap $ unlines $ map show $ st0 ^. commandHistory

  commandWindow = vLimit 3
    ( B.withFocusRing (st^.focusRing)
      (B.renderEditor $ str . unlines) (st^.commands) )

  errorWindow = vBox
    [ strWrap $ st ^. uiError
    , padTop (B.Pad 2) $ strWrap $ "(To escape this error message, "
      ++ "press Alt-e, Alt-f, Alt-d or Alt-s.)" ]

  reassuranceWindow = withAttr (B.attrName "reassurance")
    $ strWrap $ st0 ^. reassurance

  pufferWindow = error ""
--  -- TODO: Factor: This duplicates the code for resultWindow.
--  pufferWindow = viewport (BrickMainName Puffers) B.Vertical
--    $ fShow $ st ^. puffers where
--
--    fShow :: Vorest Puffer -> B.Widget BrickName
--    fShow = vBox . map vShowRec . V.toList
--    vShowOne, vShowRec :: PTree Puffer -> B.Widget BrickName
--    vShowRec bt = vShowOne bt <=>
--                  padLeft (B.Pad 2) (fShow $ bt ^. pTrees )
--    vShowOne bt = style $ strWrap $ show $ _pTreeLabel bt
--      where style :: B.Widget BrickName
--                  -> B.Widget BrickName
--            style = if not $ bt ^. pTreeHasFocus then id
--                    else visible
--                         . withAttr (B.attrName "focused result")

  resultWindow = error ""
-- --  resultWindow = viewport (BrickMainName Results) B.Vertical
-- --    $ vShowRec $ b ^. pufferView where
-- --
-- --    fShow :: Porest RsltView -> B.Widget BrickName
-- --    fShow = vBox . map vShowRec . V.toList
-- --    vShowOne, vShowRec :: PTree RsltView -> B.Widget BrickName
-- --    vShowRec vt = vShowOne vt <=>
-- --                  padLeft (B.Pad 2) (fShow $ vt ^. pMTrees )
-- --    vShowOne vt = style $ strWrap $ vShow $ _pTreeLabel vt
-- --      where style :: B.Widget BrickName
-- --                  -> B.Widget BrickName
-- --            style = if not $ vt ^. pTreeHasFocus then id
-- --                    else visible
-- --                         . withAttr (B.attrName "focused result")



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
  B.EvKey (B.KChar 'B') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ Buffers
  B.EvKey (B.KChar 'R') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ Results
  -- Brick-focus-related stuff. So far unneeded.
    -- PITFALL: The focused `Window` is distinct from the focused
    -- widget within the `mainWindow`.
    -- B.EvKey (B.KChar '\t') [] -> B.continue $ st & focusRing %~ B.focusNext
    -- B.EvKey B.KBackTab []     -> B.continue $ st & focusRing %~ B.focusPrev

  -- for debugging
  B.EvKey (B.KChar 'p') [B.MMeta] -> B.continue
    $ st & showBufferAndViewPaths

  _ -> case st ^. showingInMainWindow of
    Results -> handleKeyboard_atResults      st ev
    Buffers -> handleKeyboard_atBufferWindow st ev
    _       -> handleUncaughtInput           st ev

appHandleEvent st _ = B.continue st

appHandleEvent_puffer :: St -> B.BrickEvent BrickName e
               -> B.EventM BrickName (B.Next St)
appHandleEvent_puffer st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc [B.MMeta] -> B.halt st

  -- command window
  B.EvKey (B.KChar 'x') [B.MMeta] -> parseAndRunCommand_puffer st
  B.EvKey (B.KChar 'k') [B.MMeta] -> B.continue
    $ emptyCommandWindow st

  -- switch main window content
  B.EvKey (B.KChar 'H') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ CommandHistory
  B.EvKey (B.KChar 'B') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ Puffers
  B.EvKey (B.KChar 'R') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ Results
  -- Brick-focus-related stuff. So far unneeded.
    -- PITFALL: The focused `Window` is distinct from the focused
    -- widget within the `mainWindow`.
    -- B.EvKey (B.KChar '\t') [] -> B.continue $ st & focusRing %~ B.focusNext
    -- B.EvKey B.KBackTab []     -> B.continue $ st & focusRing %~ B.focusPrev

  -- for debugging
  B.EvKey (B.KChar 'p') [B.MMeta] -> B.continue
    $ st & showBufferAndViewPaths

  _ -> case st ^. showingInMainWindow of
    Results -> handleKeyboard_atResults_puffer      st ev
    Puffers -> handleKeyboard_atBufferWindow_puffer st ev
    _       -> handleUncaughtInput                  st ev

appHandleEvent_puffer st _ = B.continue st


appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr                  , B.white `on` B.blue)
    , (B.editFocusedAttr           , B.black `on` B.yellow)
    , (B.attrName "reassurance"    , B.black `on` B.blue)
    , (B.attrName "focused result" , B.black `on` B.green)
    ]
