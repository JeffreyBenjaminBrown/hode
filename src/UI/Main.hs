-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE ScopedTypeVariables #-}

module UI.Main where

import Lens.Micro
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

import Rslt.RTypes
import UI.IParse
import UI.ITypes
import UI.State


appDraw :: St -> [T.Widget Name]
appDraw st = [w] where
  resultWindow = F.withFocusRing (st^.focusRing)
    (E.renderEditor (str . unlines)) (st^.results)
  commandWindow = F.withFocusRing (st^.focusRing)
    (E.renderEditor (str . unlines)) (st^.commands)
  w = C.center
    $ resultWindow <=> vLimit 3 commandWindow

appHandleEvent ::
  St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appHandleEvent st (T.VtyEvent ev) = case ev of
  V.EvKey V.KEsc []         -> M.halt st
  V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
  V.EvKey V.KBackTab []     -> M.continue $ st & focusRing %~ F.focusPrev

  V.EvKey (V.KChar 'x') [V.MMeta] ->
    let cmd = unlines $ E.getEditContents $ st ^. commands
    in case pCommand (st ^. appRslt) cmd of
      Left s1  -> M.continue $ editor_replaceText results (lines s1) st
      Right c -> case runCommand c st of
        Left s2 -> M.continue $ editor_replaceText results (lines s2) st
        Right st' -> M.continue st'

  _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
    Just Results -> T.handleEventLensed
      st results E.handleEditorEvent ev
    Just Commands -> T.handleEventLensed
      st commands E.handleEditorEvent ev
    Nothing -> return st
appHandleEvent st _ = M.continue st

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr
    [ (E.editAttr,        V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

appChooseCursor ::
  St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appChooseCursor = F.focusRingCursor (^.focusRing)

app :: M.App St e Name
app = M.App
  { M.appDraw         = appDraw
  , M.appChooseCursor = appChooseCursor
  , M.appHandleEvent  = appHandleEvent
  , M.appStartEvent   = return
  , M.appAttrMap      = const appAttrMap
  }

ui :: Rslt -> IO St
ui = M.defaultMain app . initialState
