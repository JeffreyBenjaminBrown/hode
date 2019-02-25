{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module UI.Brick where

import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)


data Name = Commands
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _commands :: E.Editor String Name
       }

makeLenses ''St


_appDraw :: St -> [T.Widget Name]
_appDraw st = [ui] where
  e1 = F.withFocusRing (st^.focusRing)
    (E.renderEditor (str . unlines)) (st^.commands)
  ui = C.center e1

_appHandleEvent ::
  St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
_appHandleEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc []         -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab []     -> M.continue $ st & focusRing %~ F.focusPrev

    _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
      Just Commands -> T.handleEventLensed
        st commands E.handleEditorEvent ev
      Nothing -> return st
_appHandleEvent st _ = M.continue st

initialState :: St
initialState = 
    St (F.focusRing [Commands])
       (E.editor Commands Nothing "")  -- the Maybe is a line number limit

_appAttrMap :: A.AttrMap
_appAttrMap = A.attrMap V.defAttr
    [ (E.editAttr,        V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

_appChooseCursor ::
  St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
_appChooseCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw         = _appDraw
          , M.appChooseCursor = _appChooseCursor
          , M.appHandleEvent  = _appHandleEvent
          , M.appStartEvent   = return
          , M.appAttrMap      = const _appAttrMap
          }

main :: IO ()
main = do
    st <- M.defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.commands
