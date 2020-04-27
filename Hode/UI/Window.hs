{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Window (
    hideReassurance                         -- ^ St -> St
  , showError, showReassurance    -- ^ String -> St -> St
  , emptyLangCmdWindow                      -- ^ St -> St
  , replaceLangCmd                          -- ^ St -> St
  ) where

import qualified Data.Set                 as S
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Widgets.Edit       as B

import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views


hideReassurance :: St -> St
hideReassurance = optionalWindows
                  %~ S.delete Reassurance

showError, showReassurance :: String -> St -> St
showError msg =
  ( optionalWindows %~
    S.delete Reassurance
    . S.insert Error )
  . (uiError .~ msg)
showReassurance msg =
  ( optionalWindows %~
    S.insert Reassurance
    . S.delete Error )
  . (reassurance .~ msg)

emptyLangCmdWindow :: St -> St
emptyLangCmdWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing

-- | Replace the command shown in the `LangCmd` window with
-- the last successful search run from this `Buffer`.
-- Cannot be called from the `Cycles` `Buffer`,
-- only from an ordinary search results `Buffer`.
replaceLangCmd :: St -> St
replaceLangCmd st = maybe st f query where
  query :: Maybe String
  query = st ^? ( stGet_focusedBuffer . getBuffer_viewForkType
                  . _VFQuery . _QueryView )

  f :: String -> St
  f s = st & commands . B.editContentsL
        .~ TxZ.textZipper ["/f " ++ s] Nothing
