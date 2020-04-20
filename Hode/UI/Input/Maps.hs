{-# LANGUAGE TemplateHaskell #-}

-- | PITFALL: Vty's `Meta` modifier, at least on my system,
-- cannot be used in conjunction with certain characters, such as ';'.

module Hode.UI.Input.Maps (
    KeyCmd(..)
  , keyCmd_name, keyCmd_func, keyCmd_key, keyCmd_guide

  , universal_keyCmds_map    -- ^ M.Map V.Event
                             -- (St -> B.EventM BrickName (B.Next St))
  , bufferBuffer_keyCmds_map -- ^ M.Map V.Event
                             -- (St -> B.EventM BrickName (B.Next St))

  , resultWindow_commands -- ^ St -> M.Map V.Event
                          -- (B.EventM n         (B.Next St))
  , parseAndRunLangCmd -- ^    St ->
                           -- B.EventM BrickName (B.Next St)
  , runParsedLangCmd   -- ^ LangCmd -> St -> Either String
                          -- (B.EventM BrickName (B.Next St))
  ) where

import           Control.Lens hiding (folded)
import           Control.Lens.TH
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.List             as L
import qualified Data.Map              as M

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Graphics.Vty          as V

import Hode.PTree
import Hode.UI.BufferTree
import Hode.UI.Clipboard
import Hode.UI.CycleBuffer
import Hode.UI.ExprTree
import Hode.UI.ExprTree.Sort
import Hode.UI.Input.RunParsed
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Util
import Hode.UI.Util.String
import Hode.UI.Window

import Hode.Brick.Help
import Hode.Brick.Help.Types


data KeyCmd = KeyCmd
  { _keyCmd_name  :: String
  , _keyCmd_func :: St -> B.EventM BrickName (B.Next St)
  , _keyCmd_key  :: (V.Key, [V.Modifier])
  , _keyCmd_guide :: String }
makeLenses ''KeyCmd

paragraphs :: [String] -> String
paragraphs = concat . L.intersperse "\n\n"

paragraph :: [String] -> String
paragraph = concat . L.intersperse " "

go :: (St -> St) -> St -> B.EventM n (B.Next St)
go f = B.continue . f . hideReassurance

-- | `keyCmd_usePair kc` extracts the two fields of `kc` that the Hode UI
-- uses in its normal functioning to know what to execute in response to
-- what user input.
-- (The other two fields are used only in the interactive help.)
keyCmd_usePair :: KeyCmd -> (V.Event, St -> B.EventM BrickName (B.Next St))
keyCmd_usePair kc = ( uncurry V.EvKey $ _keyCmd_key kc
                    , _keyCmd_func kc )

-- | `keyCmd_helpPair kc` extracts the name and description of the `KeyCmd`,
-- for use in the interactive help.
-- (The other two fields are used only outside of the interactive help.)
keyCmd_helpPair :: KeyCmd -> (String, String)
keyCmd_helpPair kc = ( _keyCmd_name kc
                     , _keyCmd_guide kc )

universal_keyCmds :: [KeyCmd]
universal_keyCmds =
  [ KeyCmd { _keyCmd_name = "Quit"
           , _keyCmd_func = B.halt
           , _keyCmd_key  = (V.KEsc, [V.MMeta])
           , _keyCmd_guide = "Exit Hode." }
  , KeyCmd { _keyCmd_name = "Test key"
           , _keyCmd_func = B.continue . showReassurance "Vty saw that!"
           , _keyCmd_key  = (V.KChar '?', [V.MMeta])
           , _keyCmd_guide = "This isn't really part of the program; this is just used so I can test whether Brick has access to a certain key command on my console." }

  , KeyCmd { _keyCmd_name = "Execute."
           , _keyCmd_func = parseAndRunLangCmd
           , _keyCmd_key  = (V.KChar 'x', [V.MMeta])
           , _keyCmd_guide = paragraphs
             [ "There are two ways to control Hode: Through keyboard shortcuts, and through commands typed into the command window. Loading, saving, adding, deleting, searching and sorting are done through the command window; everything else (mostly changing the view) is done through keyboard shortcuts. After typing a command into the command window, use this key command to run it."
             , "The docs/ folder that comes with this app describes the language in detail --in particular, `docs/hash/the-hash-language.md`, and the `Language commands` section of `docs/ui.md`." ] }

  , KeyCmd { _keyCmd_name = "command history"
           , _keyCmd_func = B.continue
                            . (showingInMainWindow .~ LangCmdHistory)
                            . (showingErrorWindow .~ False )
           , _keyCmd_key  = (V.KChar 'H', [V.MMeta])
           , _keyCmd_guide = "Shows the history of commands the user has entered." }

  , KeyCmd { _keyCmd_name = "Show BufferBuffer."
           , _keyCmd_func = B.continue
             . (showingInMainWindow .~ BufferBuffer)
             . (showingErrorWindow .~ False )
           , _keyCmd_key  = (V.KChar 'B', [V.MMeta])
           , _keyCmd_guide = "In Hode, most of the time is spent looking at a `SubgraphBuffer`, which provides a view onto some of the data in your graph. Multiple `SubgraphBuffer`s can be open at once. The `BufferBuffer` provides a view of all the `SubgraphBuffer`s currently open." }

  , KeyCmd { _keyCmd_name = "show SubgraphBuffer"
           , _keyCmd_func = B.continue
                            . (showingInMainWindow .~ SubgraphBuffer)
                            . (showingErrorWindow .~ False )
           , _keyCmd_key  = (V.KChar 'R', [V.MMeta])
           , _keyCmd_guide = "A `SubgraphBuffer` provides a view of some of the data in the graph. Most of a user's time in Hode will be spent here." } ]

-- | These commands are available from any window.
universal_keyCmds_map ::
  M.Map V.Event (St -> B.EventM BrickName (B.Next St))
universal_keyCmds_map =
  M.fromList $ map keyCmd_usePair universal_keyCmds

bufferBuffer_intro :: String
bufferBuffer_intro = paragraphs
  [ paragraph
      [ "The `BufferBuffer` presents a tree* of available `SubgraphBuffer`s."
      , "The `BufferBuffer` looks similar to the `SubgraphBuffer` -- in particular, both are trees -- but whereas the `SubgraphBuffer` gives a tree of expressions in the graph, the `BufferBuffer` gives a tree of `SubgraphBuffer`s."
      , "This permits you to keep multiple views of your graph open at once and switch between them." ]
  , paragraph
      [ "One of the `SubgraphBuffer`s in the `BufferBuffer` is always \"focused\" (highlighted in blue)."
      , "The focused `SubgraphBuffer` is the one you will see when you return to the `SubgraphBuffer` view." ]
  , "----------------"
  , "*When the tree is flat, it looks like a list." ]

bufferBuffer_keyCmds :: [KeyCmd]
bufferBuffer_keyCmds =
  [ KeyCmd { _keyCmd_name = "cursor to prev"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree DirPrev
           , _keyCmd_key  = (V.KChar 'e', [V.MMeta])
           , _keyCmd_guide = paragraphs
             [ "Moves focus to the previous peer SubgraphView -- the one immediately above the currently focused one -- if it exists."
             , paragraph
               [ "PITFALL: This only moves the cursor between peers in the tree."
               , "If the focused `SubgraphBuffer` is first among its peers, this key command does nothing. In particular, it will not take you to the parent of the focused buffer. For that, use the `cursor to parent` command." ] ] }

  , KeyCmd { _keyCmd_name = "cursor to next"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree DirNext
           , _keyCmd_key  = (V.KChar 'd', [V.MMeta])
           , _keyCmd_guide = paragraphs
             [ "Moves focus to the next peer SubgraphView -- the one immediately below the currently focused one -- if it exists."
             , paragraph
               [ "PITFALL: This only moves the cursor between peers in the tree."
               , "Once you've reached the last peer, this key command will not take you anywhere."
               , "You can only travel in one of the other three directions -- to the expression before this one, or to this one's parent, or (if they exist) to one of this expression's children." ] ] }

  , KeyCmd { _keyCmd_name = "cursor to parent"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree DirUp
           , _keyCmd_key  = (V.KChar 's', [V.MMeta])
           , _keyCmd_guide = "Moves the focus to the parent of the currently focused `SubgraphBuffer`, if it exists." }

  , KeyCmd { _keyCmd_name = "cursor to child"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree DirDown
           , _keyCmd_key  = (V.KChar 'f', [V.MMeta])
           , _keyCmd_guide = "Moves the focus to one of the child of the currently focused `SubgraphBuffer`, if it has any." }

  , KeyCmd { _keyCmd_name = "nudge focused buffer up"
           , _keyCmd_func = go $ nudgeFocused_buffer DirPrev
           , _keyCmd_key  = (V.KChar 'E', [V.MMeta])
           , _keyCmd_guide = "Moves the focused `SubgraphBuffer` up by one position among its peers in the tree. That is, the focused `SubgraphBuffer` trades place with the `SubgraphBuffer` that used to precede it. If the focused `SubgraphBuffer` is already first among its peers in the tree, this command does nothing." }

  , KeyCmd { _keyCmd_name = "nudge focused buffer down"
           , _keyCmd_func = go $ nudgeFocused_buffer DirNext
           , _keyCmd_key  = (V.KChar 'D', [V.MMeta])
           , _keyCmd_guide = "Moves the focused `SubgraphBuffer` down by one position among its peers in the tree. That is, the focused `SubgraphBuffer` trades place with the `SubgraphBuffer` that used to follow it. If the focused `SubgraphBuffer` is already last among its peers in the tree, this command does nothing." }

  , KeyCmd { _keyCmd_name = "insert empty child buffer"
           , _keyCmd_func = go $ consBuffer_asChild emptySubgraphBuffer
           , _keyCmd_key  = (V.KChar 'c', [V.MMeta])
           , _keyCmd_guide = "Onsert an empty `SubgraphBuffer` into the tree of `SubgraphBuffer`s, as a child of the currently focused `SubgraphBuffer`." }

  , KeyCmd { _keyCmd_name = "insert empty peer buffer"
           , _keyCmd_func = go $ consBuffer_topNext emptySubgraphBuffer
           , _keyCmd_key  =  (V.KChar 't', [V.MMeta])
           , _keyCmd_guide = "Inserts an empty `SubgraphBuffer` as a peer of the currently focused one, just after it." }

  , KeyCmd { _keyCmd_name = "delete"
           , _keyCmd_func = go   deleteFocused_buffer
           , _keyCmd_key  = (V.KChar 'w', [V.MMeta])
           , _keyCmd_guide = "Deletes the currently focused buffer." }
  ]

-- | These commands are available from any window.
bufferBuffer_keyCmds_map ::
  M.Map V.Event (St -> B.EventM BrickName (B.Next St))
bufferBuffer_keyCmds_map =
  M.fromList $ map keyCmd_usePair bufferBuffer_keyCmds

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

  ( V.EvKey (V.KChar 'X') [V.MMeta],
    go ( stSetFocused_ViewExprNode_Tree . pTreeLabel
         . boolProps . selected %~ not ) ),

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
    go replaceLangCmd ),

  ( V.EvKey (V.KChar 'w') [V.MMeta],
    -- TODO : buggy: copies nonexistent empty lines.
    do liftIO ( toClipboard $ unlines $ focusedBufferStrings st )
       go $ showReassurance "SubgraphBuffer window copied to clipboard." ),

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

  -- THese look like they change the data but they don't.
  -- Maybe they are too confusing to be worth inclusion.
  ( V.EvKey (V.KChar 'E') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeInPTree DirPrev )
    . hideReassurance ),
  ( V.EvKey (V.KChar 'D') [V.MMeta],
    go $ ( stSet_focusedBuffer . bufferExprRowTree
           %~ nudgeInPTree DirNext )
    . hideReassurance ),

  ( V.EvKey (V.KChar 'i') [V.MMeta],
    goe $ addSelections_toSortedRegion ),
  ( V.EvKey (V.KChar 'y') [V.MMeta], -- 'y' is for "yank"
    goe $ removeSelections_fromSortedRegion ),
  ( V.EvKey (V.KChar 'k') [V.MMeta], -- next to 'l' for 'lower'
    goe $ raiseSelection_inSortedRegion ),
  ( V.EvKey (V.KChar 'l') [V.MMeta],
    goe $ lowerSelection_inSortedRegion ),

  ( V.EvKey (V.KChar 'o') [V.MMeta],
    goe $ updateBlockingCycles >=> updateCycleBuffer )
  ]
