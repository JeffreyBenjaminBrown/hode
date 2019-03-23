-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Main where

import           Control.Monad.IO.Class (liftIO)
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
import UI.Clipboard
import UI.Command
import UI.ITypes
import UI.IUtil
import UI.String
import UI.ViewTree
import UI.Window
import Util.Misc


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
-- Dach `ViewTree`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: St -> [B.Widget BrickName]
appDraw st0 = [w] where
  w = B.center ( mainWindow
                 <=> optionalWindows )

  st = st0 & stBuffer st0 .~ b where
  b = b0 & l .~ True where
    b0 = maybe err id $  st0 ^? stBuffer st where
      err = error "stBuffer returned Nothing in appDraw."
    l = bufferView . atPath (b0 ^. bufferPath) . vTreeIsFocused

  mainWindow = case st ^. showingInMainWindow of
    CommandHistory -> commandHistoryWindow
    Errors -> errorWindow
    Results -> resultWindow

  optionalWindows =
    ( if (st ^. showingOptionalWindows) M.! Reassurance
      then reassuranceWindow else emptyWidget ) <=>
    ( if (st ^. showingOptionalWindows) M.! Commands
      then commandWindow else emptyWidget )

  commandWindow, errorWindow, resultWindow, reassuranceWindow, commandHistoryWindow
    :: B.Widget BrickName
  commandWindow = vLimit 3
    ( B.withFocusRing (st^.focusRing)
      (B.renderEditor $ str . unlines) (st^.commands) )

  errorWindow = vBox
    [ strWrap $ st ^. uiError
    , padTop (B.Pad 2) $ strWrap $ "(To escape this error message, "
      ++ "press Alt-e, Alt-f, Alt-d or Alt-s.)" ]

  resultWindow = viewport (BrickMainName Results) B.Vertical
    $ showRec $ b ^. bufferView where

    showOne, showRec :: VTree RsltView -> B.Widget BrickName
    showRec vt | null $ vt ^. vTrees = showOne vt
               | True = showOne vt <=>
                 ( padLeft (B.Pad 2) $ vBox $ map showRec
                   $ V.toList $ vt ^. vTrees )
    showOne vt = style $ strWrap $ vShow $ _vTreeLabel vt
      where style :: B.Widget BrickName
                  -> B.Widget BrickName
            style = if not $ vt ^. vTreeIsFocused then id
                    else visible
                         . withAttr (B.attrName "focused result")

  commandHistoryWindow =
    strWrap $ unlines $ map show $ st0 ^. commandHistory

  reassuranceWindow = withAttr (B.attrName "reassurance") $
    strWrap $ st0 ^. reassurance

appChooseCursor :: St -> [B.CursorLocation BrickName]
                -> Maybe (B.CursorLocation BrickName)
appChooseCursor = B.focusRingCursor (^. focusRing)

appHandleEvent :: St -> B.BrickEvent BrickName e
               -> B.EventM BrickName (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc [B.MMeta] -> B.halt st

  B.EvKey (B.KChar 'h') [B.MMeta] -> B.continue $ unEitherSt st
    $ insertHosts_atFocus   st
  B.EvKey (B.KChar 'm') [B.MMeta] -> B.continue $ unEitherSt st
    $ insertMembers_atFocus st
  B.EvKey (B.KChar 'c') [B.MMeta] -> B.continue $ unEitherSt st
    $ closeSubviews_atFocus st

  B.EvKey (B.KChar 'w') [B.MMeta] -> do
    -- TODO : slightly buggy: conjures, copies some empty lines.
    liftIO ( toClipboard $ unlines $ resultsText st )
    B.continue $ st
      & showReassurance "Results window copied to clipboard."
  B.EvKey (B.KChar 'k') [B.MMeta] -> B.continue
    $ emptyCommandWindow st

  B.EvKey (B.KChar 'e') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus DirPrev
    $ st & hideReassurance
  B.EvKey (B.KChar 'd') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus DirNext
    $ st & hideReassurance
  B.EvKey (B.KChar 'f') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus DirDown
    $ st & hideReassurance
  B.EvKey (B.KChar 's') [B.MMeta] -> B.continue
    $ unEitherSt st . (moveFocus DirUp)
    $ st & hideReassurance

  B.EvKey (B.KChar 'x') [B.MMeta] -> parseAndRunCommand st

  B.EvKey (B.KChar 'H') [B.MMeta] -> B.continue
    $ st & showingInMainWindow .~ CommandHistory

  -- Window-focus-related stuff. The first two lines, which move the focus,
  -- are disabled, because so far switching focus isn't useful.
  -- PITFALL: The focused `Window` is distinct from the focused `RsltView`.
  -- B.EvKey (B.KChar '\t') [] -> B.continue $ st & focusRing %~ B.focusNext
  -- B.EvKey B.KBackTab []     -> B.continue $ st & focusRing %~ B.focusPrev
  _ -> B.continue =<< case B.focusGetCurrent $ st ^. focusRing of
    Just (BrickOptionalName Commands) -> B.handleEventLensed
      (hideReassurance st) commands B.handleEditorEvent ev
    _ -> return st
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr                  , B.white `on` B.blue)
    , (B.editFocusedAttr           , B.black `on` B.yellow)
    , (B.attrName "reassurance"    , B.black `on` B.blue)
    , (B.attrName "focused result" , B.black `on` B.green)
    ]
