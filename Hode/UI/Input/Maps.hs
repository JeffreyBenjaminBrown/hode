module Hode.UI.Input.Maps (
    universal_commands    -- ^ St -> M.Map V.Event
                          -- (B.EventM BrickName (B.Next St))
  , bufferWindow_commands -- ^ St -> M.Map V.Event
                          -- (B.EventM n         (B.Next St))
  , resultWindow_commands -- ^ St -> M.Map V.Event
                          -- (B.EventM n         (B.Next St))
  , parseAndRunCommand -- ^            St ->
                           -- B.EventM BrickName (B.Next St)
  , runParsedCommand   -- ^ Command -> St -> Either String
                          -- (B.EventM BrickName (B.Next St))
  ) where

import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map              as M
import           Lens.Micro hiding (folded)

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Graphics.Vty          as V

import Hode.PTree
import Hode.UI.ExprTree
import Hode.UI.BufferTree
import Hode.UI.Clipboard
import Hode.UI.CycleBuffer
import Hode.UI.IUtil
import Hode.UI.IUtil.String
import Hode.UI.Input.RunParsed
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window


-- | These commands are available from any window.
universal_commands ::
  St -> M.Map V.Event (B.EventM BrickName (B.Next St))
universal_commands st = M.fromList [
  ( V.EvKey V.KEsc [V.MMeta],
    B.halt st ),

  -- command window
  ( V.EvKey (V.KChar 'x') [V.MMeta],
    parseAndRunCommand st ),

  -- switch main window content
  ( V.EvKey (V.KChar 'H') [V.MMeta],
    B.continue
           $ st & showingInMainWindow .~ CommandHistory
                & showingErrorWindow .~ False ),
  ( V.EvKey (V.KChar 'B') [V.MMeta],
    B.continue
           $ st & showingInMainWindow .~ BufferBuffer
                & showingErrorWindow .~ False ),
  ( V.EvKey (V.KChar 'R') [V.MMeta],
    B.continue
           $ st & showingInMainWindow .~ SearchBuffer
                & showingErrorWindow .~ False )

  -- Brick-focus-related stuff. So far unneeded.
    -- PITFALL: The focused `Window` is distinct from
    -- the focused widget within the `mainWindow`.
    -- ( V.EvKey (V.KChar '\t') [],
    --   B.continue $ st & focusRing %~ B.focusNext ),
    -- ( V.EvKey V.KBackTab [],
    --   B.continue $ st & focusRing %~ B.focusPrev )
  ]

bufferWindow_commands ::
  St -> M.Map V.Event (B.EventM n (B.Next St))
bufferWindow_commands st =
  let go f = B.continue $ f $ st & hideReassurance
  in M.fromList [
    ( V.EvKey (V.KChar 'e') [V.MMeta],
      go $ nudgeFocus_inBufferTree DirPrev ),
    ( V.EvKey (V.KChar 'd') [V.MMeta],
      go $ nudgeFocus_inBufferTree DirNext ),
    ( V.EvKey (V.KChar 'f') [V.MMeta],
      go $ nudgeFocus_inBufferTree DirDown ),
    ( V.EvKey (V.KChar 's') [V.MMeta],
      go $ nudgeFocus_inBufferTree DirUp ),

    ( V.EvKey (V.KChar 'E') [V.MMeta],
      go $ nudgeFocused_buffer DirPrev ),
    ( V.EvKey (V.KChar 'D') [V.MMeta],
      go $ nudgeFocused_buffer DirNext ),

    ( V.EvKey (V.KChar 'c') [V.MMeta],
      go $ consBuffer_asChild emptySearchBuffer ),
    ( V.EvKey (V.KChar 't') [V.MMeta],
      go $ consBuffer_topNext emptySearchBuffer ),

    ( V.EvKey (V.KChar 'w') [V.MMeta],
      go   deleteFocused_buffer )
    ]

resultWindow_commands ::
  St -> M.Map V.Event (B.EventM n (B.Next St))
resultWindow_commands st =
  let go f = B.continue $ f st
      goe f = B.continue $ unEitherSt st $ f st
  in M.fromList [

  ( V.EvKey (V.KChar 'S') [V.MMeta],
    goe insertSearchResults_atFocus ),
  ( V.EvKey (V.KChar 'h') [V.MMeta],
    goe insertHosts_atFocus ),
  ( V.EvKey (V.KChar 'm') [V.MMeta],
    goe insertMembers_atFocus ),
  ( V.EvKey (V.KChar 'c') [V.MMeta],
    go $ stSetFocused_ViewExprNode_Tree . pMTrees .~ Nothing ),
  ( V.EvKey (V.KChar 'F') [V.MMeta],
    go ( stSetFocused_ViewExprNode_Tree . pTreeLabel
         . otherProps . folded
         %~ not ) ),

  ( V.EvKey (V.KChar 'a') [V.MMeta],
    go $ (viewOptions . viewOpt_ShowAddresses %~ not)
     . showReassurance "Toggled: show addresses to left of expressions." ),
  ( V.EvKey (V.KChar 'A') [V.MMeta],
    goe $ redraw_focusedBuffer
     . showReassurance "Toggled: replace some already-stated expressions with their addresses."
     . (viewOptions . viewOpt_ShowAsAddresses %~ not) ),

  ( V.EvKey (V.KChar 'b') [V.MMeta],
    goe cons_focusedViewExpr_asChildOfBuffer ),

  ( V.EvKey (V.KChar 'r') [V.MMeta],
    go replaceCommand ),

  ( V.EvKey (V.KChar 'w') [V.MMeta],
    -- TODO : buggy: copies nonexistent empty lines.
    do liftIO ( toClipboard $ unlines $ focusedBufferStrings st )
       go $ showReassurance "SearchBuffer window copied to clipboard." ),

  ( V.EvKey (V.KChar 'e') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeFocus_inPTree DirPrev )
    . hideReassurance ),
  ( V.EvKey (V.KChar 'd') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeFocus_inPTree DirNext )
    . hideReassurance ),
  ( V.EvKey (V.KChar 'f') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeFocus_inPTree DirDown )
    . hideReassurance ),
  ( V.EvKey (V.KChar 's') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeFocus_inPTree DirUp )
    . hideReassurance ),

  ( V.EvKey (V.KChar 'E') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeInPTree DirPrev )
    . hideReassurance ),
  ( V.EvKey (V.KChar 'D') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeInPTree DirNext )
    . hideReassurance ),

  ( V.EvKey (V.KChar 'o') [V.MMeta],
    goe $ updateBlockingCycles >=> updateCycleBuffer )
  ]
