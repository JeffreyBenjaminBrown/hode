{-# LANGUAGE ScopedTypeVariables #-}

module UI.Window (
    hideReassurance            -- ^           St -> St
  , showError, showReassurance -- ^ String -> St -> St
  , emptyCommandWindow         -- ^           St -> St
  ) where

import qualified Data.Map                 as M
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Widgets.Edit       as B

import UI.ITypes


hideReassurance :: St -> St
hideReassurance = showingOptionalWindows %~ M.insert Reassurance False

showError, showReassurance :: String -> St -> St
showError msg = (showingOptionalWindows %~ M.insert Reassurance False)
                . (showingErrorWindow .~ True)
                . (uiError .~ msg)
showReassurance msg = (showingOptionalWindows %~ M.insert Reassurance True)
                      . (showingErrorWindow .~ False)
                      . (reassurance .~ msg)

emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing
