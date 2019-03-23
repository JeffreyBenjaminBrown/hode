{-# LANGUAGE ScopedTypeVariables #-}

module UI.Window (
    hideReassurance            -- ^           St -> St
  , showError, showReassurance -- ^ String -> St -> St
  , emptyCommandWindow         -- ^           St -> St
  ) where

import qualified Data.Map                 as M
import qualified Data.Vector              as V
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Focus              as B
import qualified Brick.Widgets.Edit       as B

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc


hideReassurance :: St -> St
hideReassurance = showingOptionalWindows %~ M.insert Reassurance False

showError, showReassurance :: String -> St -> St
showError msg = (showingOptionalWindows %~ M.insert Reassurance False)
                . (showingInMainWindow .~ Errors)
                . (uiError .~ msg)
showReassurance msg = (showingOptionalWindows %~ M.insert Reassurance True)
                      . (reassurance .~ msg)

emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing
