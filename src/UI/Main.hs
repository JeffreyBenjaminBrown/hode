-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import           Lens.Micro

import qualified Brick.Main as B
import qualified Brick.Types as B
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.AttrMap as B
import qualified Brick.Focus as B
import           Brick.Util (on)
import qualified Graphics.Vty as B

import Rslt.Index (mkRslt)
import Rslt.RTypes
import UI.Clipboard
import UI.State
import UI.ITypes
import UI.IUtil
import UI.ViewTree


ui :: IO St
ui = uiFromRslt $ mkRslt mempty

uiFromSt :: St -> IO St
uiFromSt = B.defaultMain app

uiFromRslt :: Rslt -> IO St
uiFromRslt = B.defaultMain app . initialState

app :: B.App St e WindowName
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

-- | The focused subview is recalculated at each call to `appDisplay`.
-- Dach `ViewTree`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: St -> [B.Widget WindowName]
appDraw st0 = [w] where
  w = B.center $ outputWindow <=> vLimit 3 commandWindow

  st = st0 & l .~ True where
    l = viewTree . atPath (st0 ^. pathToFocus) . viewIsFocused

  outputWindow, commandWindow :: B.Widget WindowName
  outputWindow = case st ^. shownInResultsWindow of
    ShowingError -> strWrap $ st ^. uiError
    ShowingResults -> viewport Results B.Vertical
                      $ showRec $ st ^. viewTree where

      showOne, showRec :: ViewTree -> B.Widget WindowName
      showRec vt | null $ vt ^. viewSubviews = showOne vt
                 | True = showOne vt <=>
                   ( padLeft (B.Pad 2) $ vBox $ map showRec
                     $ V.toList $ vt ^. viewSubviews )
      showOne vt = style $ strWrap $ vShow $ _viewContent vt
        where style :: B.Widget WindowName -> B.Widget WindowName
              style = if not $ vt ^. viewIsFocused then id
                      else visible
                           . withAttr (B.attrName "focused result")

  commandWindow = B.withFocusRing (st^.focusRing)
    -- TODO ? There's so far never reason to focus anywhere but COmmands.
    (B.renderEditor (str . unlines)) (st^.commands)

appChooseCursor ::
  St -> [B.CursorLocation WindowName] -> Maybe (B.CursorLocation WindowName)
appChooseCursor = B.focusRingCursor (^. focusRing)

appHandleEvent ::
  St -> B.BrickEvent WindowName e -> B.EventM WindowName (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc []         -> B.halt st
  B.EvKey (B.KChar '\t') [] -> B.continue $ st & focusRing %~ B.focusNext
  B.EvKey B.KBackTab []     -> B.continue $ st & focusRing %~ B.focusPrev

  B.EvKey (B.KChar 'i') [B.MMeta] -> B.continue $ updateSt st
    $ insertHosts_atFocus st

  B.EvKey (B.KChar 'r') [B.MMeta] ->
    -- TODO : slightly buggy: conjures, copies some empty lines.
    liftIO ( toClipboard $ unlines $ resultsText st )
    >> B.continue st
  B.EvKey (B.KChar 'k') [B.MMeta] -> B.continue
    $ emptyCommandWindow st

  B.EvKey (B.KChar 'e') [B.MMeta] -> B.continue $ updateSt st
    $ moveFocus DirUp st
  B.EvKey (B.KChar 'd') [B.MMeta] -> B.continue $ updateSt st
    $ moveFocus DirDown st
  B.EvKey (B.KChar 'f') [B.MMeta] -> B.continue $ updateSt st
    $ moveFocus DirRight st
  B.EvKey (B.KChar 's') [B.MMeta] -> B.continue $ updateSt st
    $ moveFocus DirLeft st

  B.EvKey (B.KChar 'x') [B.MMeta] -> parseAndRunCommand st

  _ -> B.continue =<< case B.focusGetCurrent $ st ^. focusRing of
    Just Commands -> B.handleEventLensed 
      st commands B.handleEditorEvent ev
    _ -> return st
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr                  , B.white `on` B.blue)
    , (B.editFocusedAttr           , B.black `on` B.yellow)
    , (B.attrName "focused result" , B.black `on` B.green)
    ]
