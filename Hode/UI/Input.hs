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

  , universal_keyCmds_map      -- ^ M.Map V.Event
                               -- (St -> B.EventM BrickName (B.Next St))
  , bufferBuffer_keyCmds_map   -- ^ M.Map V.Event
                               -- (St -> B.EventM BrickName (B.Next St))
  , subgraphBuffer_keyCmds_map -- ^ M.Map V.Event
                               -- (St -> B.EventM BrickName (B.Next St))
  ) where

import qualified Data.Map              as M
import           Lens.Micro

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Brick.Widgets.Edit    as B
import qualified Brick.Focus           as B
import qualified Graphics.Vty          as V

import Hode.UI.Input.KeyMaps_and_Docs
import Hode.UI.Input.Util
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
  case M.lookup ev bufferBuffer_keyCmds_map of
  Just c -> c st
  _ -> handleUncaughtInput st ev

handleKeyboard_atResultsWindow ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atResultsWindow st ev =
  case M.lookup ev subgraphBuffer_keyCmds_map of
  Just c -> c st
  _ -> handleUncaughtInput st ev

-- TODO ? The next three definitions seem either too short,
-- or too redundant, or both.

universal_keyCmds_map ::
  M.Map V.Event (St -> B.EventM BrickName (B.Next St))
universal_keyCmds_map =
  M.fromList $ map keyCmd_usePair universal_keyCmds

bufferBuffer_keyCmds_map ::
  M.Map V.Event (St -> B.EventM BrickName (B.Next St))
bufferBuffer_keyCmds_map =
  M.fromList $ map keyCmd_usePair bufferBuffer_keyCmds

subgraphBuffer_keyCmds_map ::
  M.Map V.Event (St -> B.EventM BrickName (B.Next St))
subgraphBuffer_keyCmds_map =
  M.fromList $ map keyCmd_usePair subgraphBuffer_keyCmds
