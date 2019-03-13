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
import UI.ITypes
import UI.State
import UI.ViewTree


ui :: IO St
ui = uiFrom $ mkRslt mempty

uiFrom :: Rslt -> IO St
uiFrom = B.defaultMain app . initialState

app :: B.App St e WindowName
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

-- | The focused subview is recalculated at each call to `appDisplay`.
-- Dach `View`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: St -> [B.Widget WindowName]
appDraw st0 = [w] where
  w = B.center
    $ outputWindow <=> vLimit 3 commandWindow

  st = let
    v = either err id $ foc $ st0 ^. view where
      err = error "appDraw: todo: handle better"
      foc = mod_viewAt (st0 ^. pathToFocus) (viewIsFocused .~ True)
    in st0 & view .~ v

  outputWindow, commandWindow :: B.Widget WindowName
  outputWindow = case st ^. shownInResultsWindow of
    ShowingError -> strWrap $ st ^. uiError
    ShowingResults -> let 
      f :: View -> B.Widget WindowName
      f v = style $ strWrap $ vShow $ _viewContent v where
        style :: B.Widget WindowName -> B.Widget WindowName
        style = if not $ v ^. viewIsFocused then id
                  else withAttr $ B.attrName "focused result"

      in f (st ^. view)
         <=> ( padLeft (B.Pad 2)
               ( vBox $ map f $ V.toList
                 $ st ^. view . viewSubviews ) )

  commandWindow = B.withFocusRing (st^.focusRing)
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

  B.EvKey (B.KChar 'r') [B.MMeta] ->
    -- TODO : slightly buggy: conjures, copies some empty lines.
    liftIO ( toClipboard $ unlines $ resultsText st )
    >> B.continue st
  B.EvKey (B.KChar 'k') [B.MMeta] ->
    B.continue $ emptyCommandWindow st

  B.EvKey (B.KChar 'e') [B.MMeta] ->
    let (e :: Either String St) = moveFocus DirUp st
        st' = either errMsg id e where
          errMsg = error "appHandleEvent: todo: handle either better"
    in B.continue st'
  B.EvKey (B.KChar 'd') [B.MMeta] ->
    let (e :: Either String St) = moveFocus DirDown st
        st' = either errMsg id e where
          errMsg = error "appHandleEvent: todo: handle either better"
    in B.continue st'
  B.EvKey (B.KChar 'f') [B.MMeta] ->
    let (e :: Either String St) = moveFocus DirRight st
        st' = either errMsg id e where
          errMsg = error "appHandleEvent: todo: handle either better"
    in B.continue st'
  B.EvKey (B.KChar 's') [B.MMeta] ->
    let (e :: Either String St) = moveFocus DirLeft st
        st' = either errMsg id e where
          errMsg = error "appHandleEvent: todo: handle either better"
    in B.continue st'

  B.EvKey (B.KChar 'x') [B.MMeta] -> parseAndRunCommand st

  _ -> B.continue =<< case B.focusGetCurrent (st^.focusRing) of
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
