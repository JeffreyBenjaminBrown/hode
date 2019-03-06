-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE ScopedTypeVariables #-}

module UI.Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
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
import UI.ITypes
import UI.State


appDraw :: St -> [B.Widget Name]
appDraw st = [w] where
  w = B.center
    $ outputWindow <=> vLimit 3 commandWindow

  outputWindow, commandWindow :: B.Widget Name
  outputWindow = case st ^. showingThing of
    ShowingError -> strWrap $ st ^. uiError
    ShowingResults -> strWrap (st ^. results' . vQueryString)
      <=> padLeft (B.Pad 2) ( vBox $ map f $ M.toList
                              $ st ^. results' . vQueryResults )
      where f :: (Addr, QueryResult) -> B.Widget Name
            f (a,qr) = strWrap
              $ show a ++ ": " ++ show (qr ^. resultString)

  commandWindow = B.withFocusRing (st^.focusRing)
    (B.renderEditor (str . unlines)) (st^.commands)

appHandleEvent ::
  St -> B.BrickEvent Name e -> B.EventM Name (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc []         -> B.halt st
  B.EvKey (B.KChar '\t') [] -> B.continue $ st & focusRing %~ B.focusNext
  B.EvKey B.KBackTab []     -> B.continue $ st & focusRing %~ B.focusPrev

  B.EvKey (B.KChar 'w') [B.MMeta] -> -- copy focused window to clipboard
    liftIO ( toClipboard $ unlines $ B.getEditContents $ focusedWindow st )
    >> B.continue st
  B.EvKey (B.KChar 'r') [B.MMeta] -> -- copy results
    liftIO ( toClipboard $ unlines $ results'Text st )
    >> B.continue st
  B.EvKey (B.KChar 'k') [B.MMeta] -> -- empty the commands window
    B.continue $ editor_replaceText commands [] st

  B.EvKey (B.KChar 'x') [B.MMeta] -> parseAndRunCommand st

  _ -> B.continue =<< case B.focusGetCurrent (st^.focusRing) of
    Just Results -> B.handleEventLensed
      st results B.handleEditorEvent ev
    Just Commands -> B.handleEventLensed
      st commands B.handleEditorEvent ev
    Nothing -> return st
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr,        B.white `on` B.blue)
    , (B.editFocusedAttr, B.black `on` B.yellow)
    ]

appChooseCursor ::
  St -> [B.CursorLocation Name] -> Maybe (B.CursorLocation Name)
appChooseCursor = B.focusRingCursor (^.focusRing)

app :: B.App St e Name
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

uiFrom :: Rslt -> IO St
uiFrom = B.defaultMain app . initialState

ui :: IO St
ui = uiFrom $ mkRslt mempty
