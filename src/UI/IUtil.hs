module UI.IUtil where

import qualified Data.Vector as V
import Lens.Micro

import UI.ITypes


viewLeaf :: View -> ViewTree
viewLeaf v = ViewTree {
    _viewFocus = 0
  , _viewIsFocused = False
  , _viewContent = v
  , _viewSubviews = V.empty }

instance Show St where
  show st = "St { "
   ++ "view = "                 ++ show (st ^. viewTree)             ++ ",\n"
   ++ "pathToFocus = "          ++ show (st ^. pathToFocus)          ++ ",\n"
--   ++ "uiError = "              ++ show (st ^. uiError)              ++ ",\n"
--   ++ "commands = "             ++ show (st ^. commands)             ++ ",\n"
--   ++ "appRslt = "              ++ show (st ^. appRslt)              ++ ",\n"
--   ++ "shownInResultsWindow = " ++ show (st ^. shownInResultsWindow) ++ ",\n"
   ++ "}\n"
