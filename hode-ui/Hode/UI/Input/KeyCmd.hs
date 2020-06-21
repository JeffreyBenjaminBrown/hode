{-# LANGUAGE LambdaCase
, TupleSections #-}

-- | PITFALL: Vty's `Meta` modifier, at least on my system,
-- cannot be used in conjunction with certain characters, such as ';'.

module Hode.UI.Input.KeyCmd (
    keyPrefix -- ^ (V.Key, [V.Modifier]) -> String

  , choose_mode_intro       -- ^ String
  , choose_submode_intro    -- ^ String
  , universal_keyCmds -- ^ [KeyCmd]

  , bufferBuffer_intro   -- ^ String
  , bufferBuffer_keyCmds -- ^ [KeyCmd]

  , subgraphBuffer_intro             -- ^ String
  , subgraphBuffer_universal_keyCmds -- ^ [KeyCmd]
  , subgraphBuffer_primary_keyCmds   -- ^ [KeyCmd]
  , subgraphBuffer_sort_keyCmds      -- ^ [KeyCmd]

  , commandWindow_intro    -- ^ String
  , commandWindow_keyCmds  -- ^ [KeyCmd]

  , change_mode_keyCmds_c3              -- ^ Choice3Plist
  , change_submode_keyCmds_c3           -- ^ Choice3Plist
  , bufferBuffer_c3                     -- ^ Choice3Plist
  , subgraphBuffer_universal_keyCmds_c3 -- ^ Choice3Plist
  , subgraphBuffer_primary_keyCmds_c3   -- ^ Choice3Plist
  , subgraphBuffer_sort_keyCmds_c3      -- ^ Choice3Plist

  , module Hode.UI.Input.KeyCmd.Util
  ) where

import           Control.Lens hiding (folded)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.List.PointedList as P
import           Data.Maybe
import qualified Data.Set              as S

import qualified Brick.Main            as B
import qualified Graphics.Vty          as V

import Hode.Brick.Help.Types
import Hode.PTree
import Hode.UI.BufferTree
import Hode.UI.Clipboard
import Hode.UI.CycleBuffer
import Hode.UI.ExprTree
import Hode.UI.ExprTree.Sort
import Hode.UI.Input.KeyCmd.Util
import Hode.UI.Input.LangCmd.Run
import Hode.UI.Input.Util
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Util
import Hode.UI.Util.String
import Hode.UI.Window
import Hode.Util.Misc


keyPrefix :: (V.Key, [V.Modifier]) -> String
keyPrefix (k0,ms) = let
  e :: Show a => a -> b
  e a = error $ "keyPrefix does not yet handle " ++ show a
  in ( case ms of
         [] -> ""
         [V.MMeta] -> "M-"
         m:_ -> e m )
     ++ case k0 of V.KEsc -> "Esc"
                   V.KChar c -> [c]
                   k -> e k

prefixKeyCmdName_withKey :: KeyCmd -> KeyCmd
prefixKeyCmdName_withKey kc =
  kc { _keyCmd_name = keyPrefix (_keyCmd_key kc)
                      ++ ": "
                      ++ _keyCmd_name kc }

change_mode_keyCmds_c3, change_submode_keyCmds_c3, bufferBuffer_c3, subgraphBuffer_universal_keyCmds_c3, subgraphBuffer_primary_keyCmds_c3, subgraphBuffer_sort_keyCmds_c3 :: Choice3Plist
[   change_mode_keyCmds_c3
  , change_submode_keyCmds_c3
  , bufferBuffer_c3
  , subgraphBuffer_universal_keyCmds_c3
  , subgraphBuffer_primary_keyCmds_c3
  , subgraphBuffer_sort_keyCmds_c3
  ] =
  let hp = map keyCmd_helpPair
      i = ("Introduction",)
  in map (fromJust . P.fromList)
     [ i choose_mode_intro    : hp change_mode_keyCmds
     , i choose_submode_intro : hp change_submode_keyCmds
     , i bufferBuffer_intro   : hp bufferBuffer_keyCmds
     , i subgraphBuffer_intro : hp subgraphBuffer_universal_keyCmds
     ,                          hp subgraphBuffer_primary_keyCmds
     ,                          hp subgraphBuffer_sort_keyCmds ]

choose_mode_intro :: String
choose_mode_intro = paragraphs
  [ paragraph
    [ "These commands are always available."
    , "They let you select what window to view, and if applicable, what submode to be in." ]
  , paragraph
    [ "There are a few kinds of windows."
    , "Check the entries after this introduction for more up-to-date info, but so far the kinds of windows are these:"
    , "The Error window explains when something goes wrong."
    , "The Subgraph Window shows some of the data in your graph."
    , "The Select Subgraph Window lets you choose which Subgraph to view."
    , "The History window shows a history of commands executed in the Command Window." ]
  , paragraph
    [ "The Command Window is where you type commands."
    , "(There are also keyboard shortcuts, which don't require it, and which generally don't work unless you've hidden the Command Window.)"
    , "The Command Window is unusual, in that it is small, and (when present) overlaps the others."
    , "When you're in Subgraph Mode, for instance, you always see the Subgraph Window, but you might or might not see the Command Window, depending on whether you've opened it or not."
    ] ]

choose_submode_intro :: String
choose_submode_intro = paragraphs
  [ paragraph
    [ "These commands are always available."
    , "They let you select what submode to be in, if there is a choice." ]
  , paragraph
    [ "In some modes, there is no choice of submode."
    , "For instance, when viewing the History window, you're in History Mode, and it has no submodes."
    , "On the other hand, when viewing the Subgraph Window, you are always in Subgraph Mode, but you might be in the ViewTree submode, or you might be in the Sort submode."
    , "The choice of submode determines which commands are available. (These commands to choose mode and submode are always available)."
    ] ]

universal_keyCmds :: [KeyCmd]
universal_keyCmds =
  change_mode_keyCmds ++
  change_submode_keyCmds

change_mode_keyCmds, change_submode_keyCmds :: [KeyCmd]
[ change_mode_keyCmds,
  change_submode_keyCmds] =

  let
    mode_report :: St -> String
    mode_report st =
      " View: "
      ++ case st ^. mainWindow of
        LangCmdHistory -> "command history"
        SubgraphBuffer -> "subgraph"
        BufferBuffer -> "buffer select"
        HelpBuffer -> "Help"
      ++ ". Mode: "
      ++ case stMode st of
           BufferMode -> "buffer select."
           SubgraphMode -> "subgraph. Submode: "
                           ++ case st ^. subgraphSubmode of
                                SubgraphSubmode_primary -> "primary."
                                SubgraphSubmode_sort -> "sort."
           HelpMode -> "help."
           LangCmdMode -> "command language."
           NoMode -> paragraph [ "Nothing in particular;"
                               , "only universal commands are available." ]
  in

  map prefixKeyCmdName_withKey
  <$>

  [ -- change_mode_keyCmds
    [ KeyCmd { _keyCmd_name = "Quit"
             , _keyCmd_func = B.halt
             , _keyCmd_key  = (V.KEsc, [V.MMeta])
             , _keyCmd_guide = "Exit Hode." }

    , KeyCmd { _keyCmd_name = "Help"
             , _keyCmd_func = B.continue
               . (mainWindow .~ HelpBuffer)
             , _keyCmd_key  = (V.KChar '?', [V.MMeta])
             , _keyCmd_guide = "Brings you to this help menu." }

    , KeyCmd
      { _keyCmd_name = "Subgraph Mode"
      , _keyCmd_func = B.continue
        . (\st -> showReassurance (mode_report st) st)
        . (mainWindow .~ SubgraphBuffer)
        . (optionalWindows %~ S.delete Error)
      , _keyCmd_key  = (V.KChar 'g', [V.MMeta])
      , _keyCmd_guide = paragraphs
          [ paragraph
            [ "A `SubgraphBuffer` provides a view of some of the data in the graph. Most of a user's time in Hode will be spent here."
            , "When you first open Hode you are presented with an empty SubgraphBuffer."
            , "You'll have to either create or load some data, and then search for some of it, to populate the view."
            , "You can create multiple Subgraph buffers, and switch between them using the Select Subgraph Mode."
            ] ] }

    , KeyCmd { _keyCmd_name = "Select Subgraph"
             , _keyCmd_func = B.continue
               . (\st -> showReassurance (mode_report st) st)
               . (mainWindow .~ BufferBuffer)
               . (optionalWindows %~ S.delete Error)
             , _keyCmd_key  = (V.KChar 's', [V.MMeta])
             , _keyCmd_guide = paragraph
               [ "Multiple `SubgraphBuffer`s can be open at once."
               , "The `Select Subgraph` mode  provides a view of all the subgraphs currently open, and lets you switch between them."
               ] }

    , KeyCmd { _keyCmd_name = "Command Window"
             , _keyCmd_func = B.continue
               . (\st -> showReassurance (mode_report st) st)
               . (\st -> st & optionalWindows . at LangCmds %~
                   \case Just () -> Nothing
                         Nothing -> Just () )
             , _keyCmd_key  = (V.KChar 'c', [V.MMeta])
             , _keyCmd_guide = paragraph
               [ "This toggles the Command Window."
               , "When it is visible, you can use the UI language to do things like create or modify data, and the Hash language to describe graph operations."
               , "Every command entered into the Command Window begins with a UI language command (e.g. `/add`, to add data)."
               , "The rest of the command is usually a Hash expression, but it might be something else like an address or the name of a folder on your hard drive."
               ] }

    , KeyCmd { _keyCmd_name = "History view"
             , _keyCmd_func = B.continue
               . (\st -> showReassurance (mode_report st) st)
               . (mainWindow .~ LangCmdHistory)
               . (optionalWindows %~ S.delete Error)
             , _keyCmd_key  = (V.KChar 'h', [V.MMeta])
             , _keyCmd_guide = "Shows the history of commands the user has entered." }

  --  , KeyCmd { _keyCmd_name = "Test key"
  --           , _keyCmd_func = B.continue . showReassurance "Vty saw that!"
  --           , _keyCmd_key  = (V.KChar '?', [V.MMeta])
  --           , _keyCmd_guide = "This isn't really part of the program; this is just used so I can test whether Brick has access to a certain key command on my console." }
    ]

  ,  -- change_submode_keyCmds
    [ KeyCmd { _keyCmd_name = "Subgraph submode : primary"
             , _keyCmd_func = B.continue
               . (\st -> showReassurance (mode_report st) st)
               . (subgraphSubmode .~ SubgraphSubmode_primary)
             , _keyCmd_key  = (V.KChar 'p', [V.MMeta])
             , _keyCmd_guide = paragraph
               [ "Toggle the primary submode of Subgraph Mode."
               , "When in Subgraph Mode (because you're viewing a subgraph), this submode permits you to do most of the things that are possible without typing commands into the Command Window."
               , "Perhaps most importantly, it lets you add and delete branches of the viewtree."
               , "See the help for the Subgraph Mode for more details." ] }

    , KeyCmd { _keyCmd_name = "Subgraph submode : sort"
             , _keyCmd_func = B.continue
               . (\st -> showReassurance (mode_report st) st)
               . (subgraphSubmode .~ SubgraphSubmode_sort)
             , _keyCmd_key  = (V.KChar 's', [V.MMeta])
             , _keyCmd_guide = paragraph
               [ "Toggle sort submode of Subgraph Mode."
               , "When in Subgraph Mode (because you're viewing a subgraph), this submode permits you to rearrange the order of things without typing into the Command Window."
               , "See the help for the Subgraph Mode for more details." ] }
    ] ]

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

commandWindow_intro :: String
commandWindow_intro = paragraphs
  [ paragraph
    [ "There are two ways to control Hode: Through keyboard shortcuts, and through commands typed into the Command Window."
    , "Loading, saving, adding, deleting, searching and sorting are done through the Command Window; everything else (mostly changing the view) is done through keyboard shortcuts."
    , "After typing a command into the Command Window, use this key command to run it." ]
  , "The docs/ folder that comes with this app describes the command language in detail --in particular, `docs/hash/the-hash-language.md`, and the `Language commands` section of `docs/ui.md`."
  ]

commandWindow_keyCmds :: [KeyCmd]
commandWindow_keyCmds =
  map prefixKeyCmdName_withKey
  [ KeyCmd { _keyCmd_name = "Execute command."
           , _keyCmd_func = parseAndRunLangCmd
           , _keyCmd_key  = (V.KChar 'x', [V.MMeta])
           , _keyCmd_guide =
             "Execute the command currently shown in the Command Window." }

  , KeyCmd { _keyCmd_name = "restore last good command"
           , _keyCmd_func = go replaceLangCmd
           , _keyCmd_key  = (V.KChar 'r', [V.MMeta])
           , _keyCmd_guide = "Replace the contents of the Command Window with the last successfuly executed command." }
  ]

bufferBuffer_keyCmds :: [KeyCmd]
bufferBuffer_keyCmds =
  map prefixKeyCmdName_withKey
  [ KeyCmd { _keyCmd_name = "cursor to prev"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree ToPrev
           , _keyCmd_key  = (V.KChar 'e', [])
           , _keyCmd_guide = paragraphs
             [ "Moves focus to the previous peer SubgraphView -- the one immediately above the currently focused one -- if it exists."
             , paragraph
               [ "PITFALL: This only moves the cursor between peers in the view-tree."
               , "If the focused `SubgraphBuffer` is first among its peers, this key command does nothing."
               , "In particular, it will not take you to the parent of the focused buffer. For that, use the `cursor to parent` command." ] ] }

  , KeyCmd { _keyCmd_name = "cursor to next"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree ToNext
           , _keyCmd_key  = (V.KChar 'd', [])
           , _keyCmd_guide = paragraphs
             [ "Moves focus to the next peer SubgraphView -- the one immediately below the currently focused one -- if it exists."
             , paragraph
               [ "PITFALL: This only moves the cursor between peers in the view-tree."
               , "Once you've reached the last peer, this key command will do nothing."
               , "You can still travel in one of the other three directions -- to the expression preceding this one, or to this one's view-parent, or (if they exist) to one of this expression's view-children." ] ] }

  , KeyCmd { _keyCmd_name = "cursor to parent"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree ToRoot
           , _keyCmd_key  = (V.KChar 's', [])
           , _keyCmd_guide = "Moves the focus to the view-parent of the currently focused `SubgraphBuffer`, if it exists." }

  , KeyCmd { _keyCmd_name = "cursor to child"
           , _keyCmd_func = go $ nudgeFocus_inBufferTree ToLeaf
           , _keyCmd_key  = (V.KChar 'f', [])
           , _keyCmd_guide = "Moves the focus to one of the view-children of the currently focused `SubgraphBuffer`, if it has any." }

  , KeyCmd { _keyCmd_name = "nudge focused buffer up"
           , _keyCmd_func = go $ nudgeFocused_buffer ToPrev
           , _keyCmd_key  = (V.KChar 'E', [])
           , _keyCmd_guide = paragraphs
             [ paragraph
               [ "Moves the focused `SubgraphBuffer` up by one position among its peers in the view-tree."
               , "That is, the focused `SubgraphBuffer` trades place with the `SubgraphBuffer` that used to precede it."
               , "If the focused `SubgraphBuffer` is already first among its peers in the view-tree, this command does nothing."
               ]
             , "PITFALL: This only changes the order of expressions in the view; it does not change the data in the graph." ] }

  , KeyCmd { _keyCmd_name = "nudge focused buffer down"
           , _keyCmd_func = go $ nudgeFocused_buffer ToNext
           , _keyCmd_key  = (V.KChar 'D', [])
           , _keyCmd_guide = paragraph
             [ "Moves the focused `SubgraphBuffer` down by one position among its peers in the view-tree."
             , "That is, the focused `SubgraphBuffer` trades place with the `SubgraphBuffer` that used to follow it."
             , "If the focused `SubgraphBuffer` is already last among its peers in the view-tree, this command does nothing."
             ] }

  , KeyCmd { _keyCmd_name = "insert empty child buffer"
           , _keyCmd_func = go $ insertBuffer_asChild emptySubgraphBuffer
           , _keyCmd_key  = (V.KChar 'c', [])
           , _keyCmd_guide = "Onsert an empty `SubgraphBuffer` into the tree of `SubgraphBuffer`s, as a child of the currently focused `SubgraphBuffer`." }

  , KeyCmd { _keyCmd_name = "insert empty peer buffer"
           , _keyCmd_func = go $ insertBuffer_next emptySubgraphBuffer
           , _keyCmd_key  =  (V.KChar 'p', [])
           , _keyCmd_guide = "Inserts an empty `SubgraphBuffer` as a peer of the currently focused one, just after it." }

  , KeyCmd { _keyCmd_name = "close"
           , _keyCmd_func = go   deleteFocused_buffer
           , _keyCmd_key  = (V.KChar 'w', [])
           , _keyCmd_guide = "Closes (deletes) the currently focused buffer. Does not change the graph, just the set of views into it." }
  ]

subgraphBuffer_intro :: String
subgraphBuffer_intro = paragraphs
  [ paragraph
    [ "Most of your time using Hode will probably be spent in a `SubgraphBuffer`, which provides a view of some of your graph."
    , "To initially populate the subgraph requires running a search using the Hash language in the Command Window."
    , "(The docs/ folder that comes with this app describes the language in detail --in particular, `docs/hash/the-hash-language.md`, and the `Language commands` section of `docs/ui.md`.)"
    , "A search will populates a `SubgraphBuffer` with a list of search results." ]
  , paragraph
    [ "A populated `SubgraphBuffer` can be manipulated further using these commands."
    , "It begins as a flat list, but in fact it is a tree."
    , "When an expression in the view-tree is visited, children can be inserted beneath it."
    , "Such `view-children` bear some kind of relationship to their parent expression, which will be indicated in the view."
    , "For instance, they might be subexpression of it, or it might be a subexpression of them." ]
  ]

subgraphBuffer_universal_keyCmds :: [KeyCmd]
subgraphBuffer_universal_keyCmds =
  map prefixKeyCmdName_withKey
  [ KeyCmd { _keyCmd_name = "cursor to previous"
           , _keyCmd_func = go $ ( stSet_focusedBuffer . bufferExprRowTree
                                   %~ nudgeFocus_inPTree ToPrev )
                            . hideReassurance
           , _keyCmd_key  = (V.KChar 'e', [])
           , _keyCmd_guide = paragraphs
             [ "Moves focus to the previous peer expression -- the one immediately above the currently focused one -- if it exists."
             , paragraph
               [ "PITFALL: This only moves the cursor between peers in the view-tree."
               , "If the focused `SubgraphBuffer` is first among its peers, this key command does nothing."
               , "In particular, it will not take you to the parent of the focused buffer. For that, use the `cursor to parent` command." ] ] }

  , KeyCmd { _keyCmd_name = "cursor to next"
           , _keyCmd_func = go $ ( stSet_focusedBuffer . bufferExprRowTree
                                   %~ nudgeFocus_inPTree ToNext )
                            . hideReassurance
           , _keyCmd_key  = (V.KChar 'd', [])
           , _keyCmd_guide = paragraphs
             [ "Moves focus to the next peer expression -- the one immediately below the currently focused one -- if it exists."
             , paragraph
               [ "PITFALL: This only moves the cursor between peers in the view-tree."
               , "Once you've reached the last peer, this key command will do nothing."
               , "You can still travel in one of the other three directions -- to the previous peer expression, or to this epxression's view-parent, or (if they exist) to one of this expression's view-children." ] ] }

  , KeyCmd { _keyCmd_name = "cursor to children"
           , _keyCmd_func = go $ ( stSet_focusedBuffer . bufferExprRowTree
                                   %~ nudgeFocus_inPTree ToLeaf )
                            . hideReassurance
           , _keyCmd_key  = (V.KChar 'f', [])
           , _keyCmd_guide = "Moves the focus to one of the view-children of the currently focused expression, if it has any." }

  , KeyCmd
  -- PITFALL: Prevents the user from focusing the root of a subgraph view.
  -- Allowing that would be confusing, because the root is not visible,
  -- because it's not a common `ViewForkType`. Instead it's a `VFQuery`,
  -- which is only ever (supposed to be) at the top of the viewTree,
  -- and is only shown in the buffer select view, not the subgraph view.

      { _keyCmd_name = "cursor to parent"
      , _keyCmd_func = go $
        hideReassurance
        . \st -> let
            st' = st & stSet_focusedBuffer . bufferExprRowTree
                  %~ nudgeFocus_inPTree ToRoot
            in case st' ^? stGet_focusedBuffer
                    . bufferExprRowTree . pTreeHasFocus
               of Just False -> st'
                  _          -> st
      , _keyCmd_key  = (V.KChar 's', [])
      , _keyCmd_guide = "Moves the focus to the view-parent of the currently focused expression, if it exists." }

  , KeyCmd { _keyCmd_name = "(un)select expression"
           , _keyCmd_func = go ( stSetFocused_ViewExprNode_Tree . pTreeLabel
                                 . boolProps . selected %~ not )
           , _keyCmd_key  = (V.KChar 'x', [])
           , _keyCmd_guide = paragraph
             [ "Select an expression."
             , "One of the columns to the left of the `SubgraphBuffer` indicates an `x` next to each selected expression."
             , "There's no reason to do this except in conjunction with other commands, for instance to insert expressions into an order."
             ] }

  , KeyCmd { _keyCmd_name = "copy buffer to clipboard"
           , _keyCmd_func = \st -> do
               liftIO $ toClipboard $ unlines $ focusedBufferStrings st
               go (showReassurance "SubgraphBuffer copied to clipboard.") st
           , _keyCmd_key  = (V.KChar 'w', [])
           , _keyCmd_guide = paragraphs
             [ "Copies the contents of the `SubgraphBuffer` to the clipboard, for use in other apps."
             , "BUG : On some systems this copies extra whitespace." ] }
  ]

subgraphBuffer_primary_keyCmds :: [KeyCmd]
subgraphBuffer_primary_keyCmds =
  map prefixKeyCmdName_withKey
  [ KeyCmd { _keyCmd_name = "insert host relationships"
           , _keyCmd_func = goe insertHosts_atFocus
           , _keyCmd_key  = (V.KChar 'h', [])
           , _keyCmd_guide = paragraph
             [ "The focused expression might be a member of other expressions."
             , "If so, this command will find those host relationships, and insert them into the view, as children of the focused epxression."
             ] }

  , KeyCmd { _keyCmd_name = "insert members"
           , _keyCmd_func = goe insertMembers_atFocus
           , _keyCmd_key  = (V.KChar 'm', [])
           , _keyCmd_guide = paragraph
             [ "The focused expression might be a relationship."
             , "If so, it contains sub-expressions."
             , "This command will find those member expressions, and insert them into the view, as children of the focused epxression."
             ] }

  , KeyCmd { _keyCmd_name = "insert search results"
           , _keyCmd_func = goe insertSearchResults_atFocus
           , _keyCmd_key  = (V.KChar 'S', [])
           , _keyCmd_guide = paragraphs
             [ paragraph
               [ "An expression in the graph can be interpreted as representing a search over the graph."
               , "This command does so, and inserts the results of searching for that expression as its children." ]
             , paragraph
               [ "Top-level joints in the searched-for expression are removed before searching."
               , "Thus, if the expression `a # \"|\" # b` is in your graph, focusing on (highlighting) that expression and evaluating this command will cause Hode to find what it would find if you evaluated \"/f a | b\", and insert those as children of the focused expression." ]
             , "See the section of `docs/ui.md` entitled \"Insert results of evaluating focus as a search\" for a longer discussion."
             ] }

  , KeyCmd { _keyCmd_name = "(un)fold descendents"
           , _keyCmd_func = go ( stSetFocused_ViewExprNode_Tree . pTreeLabel
                                 . otherProps . folded %~ not )
           , _keyCmd_key  = (V.KChar 'F', [])
           , _keyCmd_guide = paragraph
             [ "Folding hides the view-descendents of the focused node."
             , "Unfolding replaces them."
             , "Neither operation changes the graph, just the current view of it." ] }

  , KeyCmd { _keyCmd_name = "close descendents"
           , _keyCmd_func = go $
             stSetFocused_ViewExprNode_Tree . pMTrees .~ Nothing
           , _keyCmd_key  = (V.KChar 'k', [])
           , _keyCmd_guide = paragraph
             [ "Removes all view-descendents of the focused expression, if any exist."
             , "Does not change the graph, just the present view of it."
             , "Note that they can also be `folded`, which is less destructive."
             , "If you've carefully built up an elaborate tree to display your data, and want to temporarily hide it, you'll want to fold it, rather than close it."
             ] }

  , KeyCmd { _keyCmd_name = "show addresses"
           , _keyCmd_func =
             go $ (viewOptions . viewOpt_ShowAddresses %~ not)
             . showReassurance "Toggled: show addresses to left of expressions."
           , _keyCmd_key  = (V.KChar 'a', [])
           , _keyCmd_guide = "Precedes each expression with its address." }

  , KeyCmd { _keyCmd_name = "replace with addresses"
           , _keyCmd_func =
             goe $ redraw_focusedBuffer
             . showReassurance "Toggled: replace some already-stated expressions with their addresses."
             . (viewOptions . viewOpt_ShowAsAddresses %~ not)
           , _keyCmd_key  = (V.KChar 'A', [])
           , _keyCmd_guide = paragraph
             [ "When a long expression is a subexpression of its view-children, those view-children can become hard to read."
             , "This replaces each such subexpression with its address, which can reduce redundancy and increase readability."
             ] }

  , KeyCmd { _keyCmd_name = "new buffer at focus"
           , _keyCmd_func = goe insert_focusedViewExpr_asChildOfBuffer
           , _keyCmd_key  = (V.KChar 'b', [])
           , _keyCmd_guide = "Create a new `SubgraphBuffer` from the focused expression and its view-descendents." }

  , KeyCmd { _keyCmd_name = "nudge up in view"
           , _keyCmd_func = go $ ( stSet_focusedBuffer . bufferExprRowTree
                                   %~ nudgeInPTree ToPrev )
                            . hideReassurance
           , _keyCmd_key  = (V.KChar 'E', [])
           , _keyCmd_guide = paragraphs
             [ paragraph
               [ "Moves the focused expression up by one position among its peers in the view-tree."
               , "That is, the focused expression trades place with the one that used to precede it."
               , "If the focused expression is already first among its peers in the view-tree, this command does nothing." ]
             , "PITFALL: This only changes the order of expressions in the view; it does not change the data in the graph." ] }

  , KeyCmd { _keyCmd_name = "nudge down in view"
           , _keyCmd_func = go $ ( stSet_focusedBuffer . bufferExprRowTree
                                   %~ nudgeInPTree ToPrev )
                            . hideReassurance
           , _keyCmd_key  = (V.KChar 'D', [])
           , _keyCmd_guide = paragraphs
             [ paragraph
               [ "Moves the focused expression up by one position among its peers in the view-tree."
               , "That is, the focused expression trades place with the one that used to follow it."
               , "If the focused expression is already last among its peers in the view-tree, this command does nothing." ]
             , "PITFALL: This only changes the order of expressions in the view; it does not change the data in the graph." ] }
  ]

subgraphBuffer_sort_keyCmds :: [KeyCmd]
subgraphBuffer_sort_keyCmds =
  map prefixKeyCmdName_withKey

  [ KeyCmd { _keyCmd_name = "insert into order"
           , _keyCmd_func = goe $ addSelections_toSortedRegion
           , _keyCmd_key  = (V.KChar 'i', [])
           , _keyCmd_guide = paragraphs
             [ "Inserts those selected expressions which are not already part of the order into the order."
             , "PITFALL: This changes the graph, not just the view." ] }

  , KeyCmd { _keyCmd_name = "remove from order"
           , _keyCmd_func = goe $ removeSelections_fromSortedRegion
           , _keyCmd_key  = (V.KChar 'r', [])
           , _keyCmd_guide = paragraphs
             [ "Removes those selected expressions which are in the order from the order. This changes the graph, not just the view."
             , "PITFALL: This changes the graph, not just the view." ] }

  , KeyCmd { _keyCmd_name = "raise in order"
           , _keyCmd_func = goe $ raiseSelection_inSortedRegion
           , _keyCmd_key  = (V.KChar 'E', [])
           , _keyCmd_guide = paragraphs
             [ paragraph
               [ "Raises a group of contiguous selected expressions in the order by one position."
               , "For instance, if the order reads \"A > B > C > D > E\", and C and D are selected, the order after running this command will be \"A > C > D > B > E\"." ]
             , "PITFALL: This changes the graph, not just the view." ] }

  , KeyCmd { _keyCmd_name = "lower in order"
           , _keyCmd_func = goe $ lowerSelection_inSortedRegion
           , _keyCmd_key  = (V.KChar 'D', [])
           , _keyCmd_guide = paragraphs
             [ paragraph
               [ "Lowers a group of contiguous selected expressions in the order by one position."
               , "For instance, if the order reads \"A > B > C > D > E\", and B and C are selected, the order after running this command will be \"A > D > B > C > E\"." ]
             , "PITFALL: This changes the graph, not just the view." ] }

  , KeyCmd { _keyCmd_name = "update cycle buffer"
           , _keyCmd_func =
               goe $ updateBlockingCycles >=> updateCycleBuffer
           , _keyCmd_key  = (V.KChar 'u', [])
           , _keyCmd_guide = paragraph
             [ "When Hode detects a cycle in a transitive relationship, it suspends normal operation and displays the cycle in a `CycleBuffer`, and asks the user to break the cycle somewhere."
             , "Once the cycle is broken, running this command will cause Hode to determine if there are any more cycles." ] }
  ]
