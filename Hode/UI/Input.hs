{-# LANGUAGE ScopedTypeVariables #-}

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
import Hode.Qseq.QTypes
import Hode.Rslt.Edit
import Hode.Rslt.Files
import Hode.Rslt.RTypes
import Hode.UI.BufferTree
import Hode.UI.Clipboard
import Hode.UI.ITypes
import Hode.UI.IUtil
import Hode.UI.Input.IParse
import Hode.UI.IUtil.String
import Hode.UI.BufferRowTree
import Hode.UI.Window
import Hode.Util.Direction
import Hode.Util.Misc
import Hode.Util.PTree


handleUncaughtInput :: St -> V.Event -> B.EventM BrickName (B.Next St)
handleUncaughtInput st ev =
  B.continue =<< case B.focusGetCurrent $ st ^. focusRing of
    Just (BrickOptionalName Commands) -> B.handleEventLensed
      (hideReassurance st) commands B.handleEditorEvent ev
    _ -> return st

handleKeyboard_atBufferWindow :: St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atBufferWindow st ev = case ev of
  V.EvKey (V.KChar 'e') [V.MMeta] -> B.continue
    $ moveFocusedBuffer DirPrev
    $ st & hideReassurance
  V.EvKey (V.KChar 'd') [V.MMeta] -> B.continue
    $ moveFocusedBuffer DirNext
    $ st & hideReassurance
  V.EvKey (V.KChar 'f') [V.MMeta] -> B.continue
    $ moveFocusedBuffer DirDown
    $ st & hideReassurance
  V.EvKey (V.KChar 's') [V.MMeta] -> B.continue
    $ moveFocusedBuffer DirUp
    $ st & hideReassurance

  V.EvKey (V.KChar 'c') [V.MMeta] -> B.continue
    $ consBuffer_asChild emptyBuffer
    $ st & hideReassurance
  V.EvKey (V.KChar 't') [V.MMeta] -> B.continue
    $ consBuffer_topNext emptyBuffer
    $ st & hideReassurance

  _ -> handleUncaughtInput st ev

handleKeyboard_atResultsWindow :: St -> V.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atResultsWindow st ev = case ev of
  V.EvKey (V.KChar 'h') [V.MMeta] -> B.continue $ unEitherSt st
    $ insertHosts_atFocus   st
  V.EvKey (V.KChar 'm') [V.MMeta] -> B.continue $ unEitherSt st
    $ insertMembers_atFocus st
  V.EvKey (V.KChar 'c') [V.MMeta] -> B.continue
    $ closeSubviews_atFocus st
  V.EvKey (V.KChar 'F') [V.MMeta] -> B.continue
    $ foldSubviews_atFocus st

  V.EvKey (V.KChar 'b') [V.MMeta] -> B.continue
    $ unEitherSt st
    $ st & cons_focusedViewExpr_asChildOfBuffer

  V.EvKey (V.KChar 'r') [V.MMeta] -> B.continue
    $ replaceCommand st

  V.EvKey (V.KChar 'w') [V.MMeta] -> do
    -- TODO : slightly buggy: conjures, copies some empty lines.
    liftIO ( toClipboard $ unlines $ resultsText st )
    B.continue $ st
      & showReassurance "Results window copied to clipboard."

  V.EvKey (V.KChar 'e') [V.MMeta] -> B.continue
    $ moveFocusedViewExprNode DirPrev
    $ st & hideReassurance
  V.EvKey (V.KChar 'd') [V.MMeta] -> B.continue
    $ moveFocusedViewExprNode DirNext
    $ st & hideReassurance
  V.EvKey (V.KChar 'f') [V.MMeta] -> B.continue
    $ moveFocusedViewExprNode DirDown
    $ st & hideReassurance
  V.EvKey (V.KChar 's') [V.MMeta] -> B.continue
    $ moveFocusedViewExprNode DirUp
    $ st & hideReassurance

  _ -> handleUncaughtInput st ev

parseAndRunCommand :: St -> B.EventM BrickName (B.Next St)
parseAndRunCommand st =
  let cmd = unlines $ B.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left parseErr -> B.continue $ unEitherSt st $ Left parseErr
      -- PITFALL: these two Lefts have different types.
    Right parsedCmd -> case runParsedCommand parsedCmd st of
      Left runErr -> B.continue $ unEitherSt st $ Left runErr
        -- PITFALL: these two Lefts have different types.
      Right evNextSt -> (fmap $ fmap $ commandHistory %~ (:) parsedCmd)
                        evNextSt
        -- PITFALL: Don't call `unEitherSt` on this `evNextSt`, because
        -- it might be showing errors, because the load and save commnads
        -- must return Right in order to perform IO.


-- | Pitfall: this looks like it could just return `St` rather
-- than `Event ... St`, but it needs IO to load and save.
-- (If I really want to keep it pure I could add a field in St
-- that keeps a list of actions to execute.)
runParsedCommand ::
  Command -> St -> Either String (B.EventM BrickName (B.Next St))
runParsedCommand c0 st0 = prefixLeft "runParsedCommand:"
                          $ g c0 st0
  where

  g (CommandFind s h) st =
    prefixLeft " called on CommandFind:" $ do
    let r :: Rslt = st ^. appRslt
    as :: [Addr] <-
      S.toList <$> hExprToAddrs r (mempty :: Subst Addr) h
    let p :: Porest BufferRow
          = mkBufferRowPorest r as

    Right $ B.continue $ st
      & showingInMainWindow .~ Results
      & showingErrorWindow .~ False
      & (let strip :: String -> String
             strip = T.unpack . T.strip . T.pack
         in stSetFocusedBuffer . bufferQuery .~ strip s)
      & stSetFocusedBuffer . bufferRowPorest . _Just .~ p
      & ( stSetFocusedBuffer . bufferRowPorest . _Just .
          P.focus . pTreeHasFocus .~ True )

  g (CommandReplace a e) st =
    either Left (Right . f)
    $ replaceExpr a e (st ^. appRslt)
    where f :: Rslt -> B.EventM BrickName (B.Next St)
          f r = B.continue $ st & appRslt .~ r
                                & showingErrorWindow .~ False
                                & showReassurance msg
                                & showingInMainWindow .~ Results
            where msg = "Replaced Expr at " ++ show a ++ "."

  g (CommandDelete a) st =
    either Left (Right . f)
    $ delete a (st ^. appRslt)
    where f :: Rslt -> B.EventM BrickName (B.Next St)
          f r = B.continue $ st & appRslt .~ r
                                & showingErrorWindow .~ False
                                & showReassurance msg
            where msg = "Deleted Expr at " ++ show a ++ "."

  g (CommandInsert e) st =
    either Left (Right . f)
    $ exprToAddrInsert (st ^. appRslt) e
    where
      f :: (Rslt, [Aged Addr]) -> B.EventM BrickName (B.Next St)
      f (r,as) = B.continue $ st & appRslt .~ r
                 & showingErrorWindow .~ False
                 & showReassurance ("Exprs added at " ++
                                    show (catNews as))
                 & showingInMainWindow .~ Results

  g (CommandLoad f) st = Right $ do
    (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
    if bad
      then B.continue $ st & showError ("Non-existent folder: " ++ f)
      else do r <- liftIO $ readRslt f
              B.continue $ st & appRslt .~ r
                              & showReassurance "Rslt loaded."
                              & showingInMainWindow .~ Results
                              & showingErrorWindow .~ False

  g (CommandSave f) st = Right $ do
    (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
    st' <- if bad
      then return $ st & showError ("Non-existent folder: " ++ f)
      else do liftIO $ writeRslt f $ st ^. appRslt
              return $ st & showingInMainWindow .~ Results
                          & showingErrorWindow .~ False
                          & showReassurance "Rslt saved."
    B.continue st'
