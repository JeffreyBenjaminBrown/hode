{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Window (
    hideReassurance                         -- ^ St -> St
  , showError, showReassurance    -- ^ String -> St -> St
  , emptyCommandWindow                      -- ^ St -> St
  , replaceCommand                          -- ^ St -> St
  ) where

import qualified Data.Map                 as M
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Widgets.Edit       as B

import Hode.UI.Types.Names
import Hode.UI.Types.State


hideReassurance :: St -> St
hideReassurance = showingOptionalWindows
                  %~ M.insert Reassurance False

showError, showReassurance :: String -> St -> St
showError msg =
  (showingOptionalWindows %~ M.insert Reassurance False)
  . (showingErrorWindow .~ True)
  . (uiError .~ msg)
showReassurance msg =
  (showingOptionalWindows %~ M.insert Reassurance True)
  . (showingErrorWindow .~ False)
  . (reassurance .~ msg)

emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing

-- | Replace the command shown in the `Command` window with
-- the last successful search run from this buffer.
replaceCommand :: St -> St
replaceCommand st = maybe st f query where
  query :: Maybe String
  query = st ^? stGet_focusedBuffer . _Just . bufferQuery

  f :: String -> St
  f s = st & commands . B.editContentsL
        .~ TxZ.textZipper ["/f " ++ s] Nothing
