{-# LANGUAGE ScopedTypeVariables
, LambdaCase
, TupleSections #-}

module Hode.UI.Input (
    handleUncaughtInput            -- ^ St -> V.Event ->
                                   -- B.EventM BrickName (B.Next St)
  , handleKeyboard_atResultsWindow -- ^ St -> V.Event ->
                                   -- B.EventM BrickName (B.Next St)
  , handleKeyboard_atBufferWindow  -- ^ St -> V.Event ->
                                   -- B.EventM BrickName (B.Next St)
  ) where

import qualified Data.Map              as M
import           Lens.Micro

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Brick.Widgets.Edit    as B
import qualified Brick.Focus           as B
import qualified Graphics.Vty          as V

import Hode.UI.Input.Maps
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Window


handleUncaughtInput ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleUncaughtInput st ev =
  B.continue =<< case B.focusGetCurrent $ st ^. focusRing of
    Just (BrickOptionalName LangCmds) ->
      -- pipe user input into the LangCmds window
      B.handleEventLensed
      (hideReassurance st) commands B.handleEditorEvent ev
    _ -> return st

handleKeyboard_atBufferWindow ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atBufferWindow st ev =
  case M.lookup ev $ bufferWindow_commands st of
  Just c -> c
  _ -> handleUncaughtInput st ev

handleKeyboard_atResultsWindow ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atResultsWindow st ev =
  case M.lookup ev $ resultWindow_commands st of
  Just c -> c
  _ -> handleUncaughtInput st ev
