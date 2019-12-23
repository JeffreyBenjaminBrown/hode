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
  , parseAndRunCommand             -- ^ St ->
                                   -- B.EventM BrickName (B.Next St)
  , runParsedCommand    -- ^ Command -> St ->
                    -- Either String (B.EventM BrickName (B.Next St))
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.List.PointedList as P
import qualified Data.Set              as S
import qualified Data.Text             as T
import           Lens.Micro
import           System.Directory

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Brick.Widgets.Edit    as B
import qualified Brick.Focus           as B
import qualified Graphics.Vty          as V

import Hode.Hash.HLookup
import Hode.Rslt.Edit
import Hode.Rslt.Files
import Hode.Rslt.Sort
import Hode.Rslt.RTypes
import Hode.UI.BufferTree
import Hode.UI.Clipboard
import Hode.UI.CycleBreaker
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.IUtil
import Hode.UI.Input.IParse
import Hode.UI.IUtil.String
import Hode.UI.BufferRowTree
import Hode.UI.Window
import Hode.Util.Misc
import Hode.PTree.Initial


handleUncaughtInput ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleUncaughtInput st ev =
  B.continue =<< case B.focusGetCurrent $ st ^. focusRing of
    Just (BrickOptionalName Commands) ->
      -- pipe user input into the Commands window
      B.handleEventLensed
      (hideReassurance st) commands B.handleEditorEvent ev
    _ -> return st

handleKeyboard_atBufferWindow ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atBufferWindow st ev =
  let go f = B.continue $ f $ st & hideReassurance
  in case ev of

  V.EvKey (V.KChar 'e') [V.MMeta] ->
    go $ nudgeFocus_inBufferTree DirPrev
  V.EvKey (V.KChar 'd') [V.MMeta] ->
    go $ nudgeFocus_inBufferTree DirNext
  V.EvKey (V.KChar 'f') [V.MMeta] ->
    go $ nudgeFocus_inBufferTree DirDown
  V.EvKey (V.KChar 's') [V.MMeta] ->
    go $ nudgeFocus_inBufferTree DirUp

  V.EvKey (V.KChar 'E') [V.MMeta] ->
    go $ nudgeFocused_buffer DirPrev
  V.EvKey (V.KChar 'D') [V.MMeta] ->
    go $ nudgeFocused_buffer DirNext

  V.EvKey (V.KChar 'c') [V.MMeta] ->
    go $ consBuffer_asChild emptyBuffer
  V.EvKey (V.KChar 't') [V.MMeta] ->
    go $ consBuffer_topNext emptyBuffer

  V.EvKey (V.KChar 'w') [V.MMeta] ->
    go   deleteFocused_buffer

  _ -> handleUncaughtInput st ev

handleKeyboard_atResultsWindow ::
  St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atResultsWindow st ev =
  let go f = B.continue $ f st
      goe f = B.continue $ unEitherSt st $ f st
  in case ev of

  V.EvKey (V.KChar 'S') [V.MMeta] -> goe insertSearchResults_atFocus
  V.EvKey (V.KChar 'h') [V.MMeta] -> goe insertHosts_atFocus
  V.EvKey (V.KChar 'm') [V.MMeta] -> goe insertMembers_atFocus
  V.EvKey (V.KChar 'c') [V.MMeta] -> go closeSubviews_atFocus
  V.EvKey (V.KChar 'F') [V.MMeta] -> go foldSubviews_atFocus

  V.EvKey (V.KChar 'a') [V.MMeta] ->
    go $ (viewOptions . viewOpt_ShowAddresses %~ not)
     . showReassurance "Toggled: show addresses to left of expressions."
  V.EvKey (V.KChar 'A') [V.MMeta] ->
    goe $ redraw_focusedBuffer
     . showReassurance "Toggled: replace some already-stated expressions with their addresses."
     . (viewOptions . viewOpt_ShowAsAddresses %~ not)

  V.EvKey (V.KChar 'b') [V.MMeta] ->
    goe cons_focusedViewExpr_asChildOfBuffer

  V.EvKey (V.KChar 'r') [V.MMeta] -> go replaceCommand

  V.EvKey (V.KChar 'w') [V.MMeta] -> do
    -- TODO : buggy: copies nonexistent empty lines.
    liftIO ( toClipboard $ unlines $ focusedBufferStrings st )
    go $ showReassurance "Results window copied to clipboard."

  V.EvKey (V.KChar 'e') [V.MMeta] -> go $
    nudgeFocus_inViewExprNodes DirPrev . hideReassurance
  V.EvKey (V.KChar 'd') [V.MMeta] -> go $
    nudgeFocus_inViewExprNodes DirNext . hideReassurance
  V.EvKey (V.KChar 'f') [V.MMeta] -> go $
    nudgeFocus_inViewExprNodes DirDown . hideReassurance
  V.EvKey (V.KChar 's') [V.MMeta] -> go $
    nudgeFocus_inViewExprNodes DirUp .   hideReassurance

  V.EvKey (V.KChar 'E') [V.MMeta] -> go $
    nudge_viewExprNode DirPrev . hideReassurance
  V.EvKey (V.KChar 'D') [V.MMeta] -> go $
    nudge_viewExprNode DirNext . hideReassurance

  V.EvKey (V.KChar 'o') [V.MMeta] ->
    goe setCycleBuffer

  _ -> handleUncaughtInput st ev

parseAndRunCommand ::
  St -> B.EventM BrickName (B.Next St)
parseAndRunCommand st =
  let cmd :: String = unlines $ B.getEditContents
                      $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left parseErr ->
      B.continue $ unEitherSt st $ Left parseErr
    Right (parsedCmd :: Command) ->
      case runParsedCommand parsedCmd st of
        Left runErr ->
          B.continue $ unEitherSt st $ Left runErr
        Right (evNextSt :: B.EventM BrickName (B.Next St)) ->
          (fmap $ fmap $ commandHistory %~ (:) parsedCmd)
          evNextSt
        -- PITFALL: Don't call `unEitherSt` on this `evNextSt`, because
        -- it might be showing errors, because the load and save commnads
        -- must return Right in order to perform IO.

-- | Pitfall: this looks like it could just return `St` rather
-- than `EventM ... St`, but it needs IO to load and save.
-- (If I really want to keep it pure I could add a field in St
-- that keeps a list of actions to execute.)
runParsedCommand ::
  Command -> St ->
  Either String (B.EventM BrickName (B.Next St))
runParsedCommand                     c0 st0 =
  prefixLeft "runParsedCommand:" $ g c0 st0
  where

  itWorked :: String -> St -> St
  itWorked s = showReassurance s
               . (showingInMainWindow .~ Results)

  -- todo ? duplicative of the clause for CommandInsert
  g (CommandReplace a e) st = do
    (r :: Rslt, cs :: [Cycle]) <-
      replaceExpr a e (st ^. appRslt)
    let st1 = st & appRslt .~ r
    if null cs
      then Right . B.continue $ st1
           & itWorked ( "Replaced Expr at "
                        ++ show a ++ "." )
      else B.continue <$> setCycleBuffer
           (st1 & stSetFocusedBuffer . bufferCycles .~ cs)

  -- For mocking purpsoses only.
  g (CommandAddCycle as) st =
    Right $ B.continue $ st
      & stSetFocusedBuffer . bufferCycles %~ (as :)
      & itWorked "Added cycle to buffer."

  g (CommandMove old new) st = do
    r <- moveRefExpr old new (st ^. appRslt)
    Right $ B.continue $ st
      & appRslt .~ r
      & itWorked ( "Moved Expr from " ++ show old
                   ++ " to " ++ show new ++ "." )

  g (CommandDelete a) st = do
    r <- deleteIfUnused a (st ^. appRslt)
    Right $ B.continue $ st & appRslt .~ r
      & itWorked ( "Deleted Expr at "
                   ++ show a ++ "." )

  -- todo ? duplicative of the clause for CommandReplace
  g (CommandInsert e) st = do
    (r :: Rslt, as :: [Aged Addr], cs :: [Cycle]) <-
      exprToAddrInsert (st ^. appRslt) e
    let st1 = st & appRslt .~ r
    if null cs
      then Right $ B.continue $ st1
           & itWorked ( "Expr(s) added at " ++
                        show (catNews as) )
      else B.continue <$> setCycleBuffer
           (st1 & stSetFocusedBuffer . bufferCycles .~ cs)

  g (CommandLoad f) st = Right $ do
    (bad :: Bool) <-
      liftIO $ not <$> doesDirectoryExist f
    if bad
      then B.continue $ st
           & showError ("Non-existent folder: " ++ f)
      else do
      r <- liftIO $ readRslt f
      B.continue $ st & appRslt .~ r
        & itWorked "Rslt loaded."

  g (CommandSave f) st = Right $ do
    (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
    st' <- if bad
      then return $ st & showError
           ("Non-existent folder: " ++ f)
      else do
      liftIO $ writeRslt f $ st ^. appRslt
      return $ st & itWorked "Rslt saved."
    B.continue st'

  g cmd st =
    prefixLeft "called to find and maybe sort:" $ do
    let r :: Rslt = st ^. appRslt
    (s :: String, as :: [Addr]) <- case cmd of
      CommandFind     s h      ->
        (s,) <$>
        ( S.toList <$> hExprToAddrs r mempty h )
      CommandFindSort s h bo t ->
        (s,) <$>
        ( S.toList <$> hExprToAddrs r mempty h
          >>= kahnSort r (bo,t) )
      _ -> Left "This should be impossible -- the other Commands have already been handled by earlier clauses defining `g`."
    p :: Porest BufferRow <- -- new screen to show
      (P.focus . pTreeHasFocus .~ True)
      <$> addrsToBufferRows st mempty as

    Right $ B.continue $ st
      & ( ( -- PITFALL : Replaces the Buffer's old contents.
            -- TODO ? Create a new Buffer instead.
            let strip :: String -> String
                strip = T.unpack . T.strip . T.pack
            in stSetFocusedBuffer . bufferQuery .~ strip s )
          . ( stSetFocusedBuffer . bufferRowPorest . _Just
              .~ p ) )
      & itWorked "Search successful."
