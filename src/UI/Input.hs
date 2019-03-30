{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.Input (
    handleUncaughtInput                  -- ^ St -> B.Event ->
                                         -- B.EventM BrickName (B.Next St)
  , handleKeyboard_atResults_puffer      -- ^ St -> B.Event ->
                                         -- B.EventM BrickName (B.Next St)
  , handleKeyboard_atBufferWindow_puffer -- ^ St -> B.Event ->
                                         -- B.EventM BrickName (B.Next St)
  , parseAndRunCommand_puffer            -- ^ St ->
                                         -- B.EventM BrickName (B.Next St)
  , runParsedCommand_puffer -- ^ Command -> St ->
                            -- Either String (B.EventM BrickName (B.Next St))
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.List.PointedList as P
import           Data.Set (Set)
import qualified Data.Set              as S
import qualified Data.Vector           as V
import           Lens.Micro
import           System.Directory

import qualified Brick.Main            as B
import qualified Brick.Types           as B
import qualified Brick.Widgets.Edit    as B
import qualified Brick.Focus           as B
import qualified Graphics.Vty          as B

import Hash.HLookup
import Qseq.QTypes
import Rslt.Edit
import Rslt.Files
import Rslt.RTypes
import UI.BufferTree
import UI.Clipboard
import UI.ITypes
import UI.IUtil
import UI.Input.IParse
import UI.String
import UI.RsltViewTree
import UI.Window
import Util.Direction
import Util.Misc
import Util.PTree
import Util.VTree


handleUncaughtInput :: St -> B.Event -> B.EventM BrickName (B.Next St)
handleUncaughtInput st ev =
  B.continue =<< case B.focusGetCurrent $ st ^. focusRing of
    Just (BrickOptionalName Commands) -> B.handleEventLensed
      (hideReassurance st) commands B.handleEditorEvent ev
    _ -> return st

handleKeyboard_atBufferWindow_puffer :: St -> B.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atBufferWindow_puffer st ev = case ev of
  B.EvKey (B.KChar 'e') [B.MMeta] -> B.continue
    $ moveFocusedPuffer DirPrev
    $ st & hideReassurance
  B.EvKey (B.KChar 'd') [B.MMeta] -> B.continue
    $ moveFocusedPuffer DirNext
    $ st & hideReassurance
  B.EvKey (B.KChar 'f') [B.MMeta] -> B.continue
    $ moveFocusedPuffer DirDown
    $ st & hideReassurance
  B.EvKey (B.KChar 's') [B.MMeta] -> B.continue
    $ moveFocusedPuffer DirUp
    $ st & hideReassurance

  B.EvKey (B.KChar 'c') [B.MMeta] -> B.continue
    $ consPufferAsChild emptyPuffer
    $ st & hideReassurance
  B.EvKey (B.KChar 't') [B.MMeta] -> B.continue
    $ consPufferAtTop emptyPuffer
    $ st & hideReassurance

  _ -> handleUncaughtInput st ev

handleKeyboard_atResults_puffer :: St -> B.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atResults_puffer st ev = case ev of
  B.EvKey (B.KChar 'h') [B.MMeta] -> B.continue $ unEitherSt st
    $ insertHosts_atFocus_puffer   st
  B.EvKey (B.KChar 'm') [B.MMeta] -> B.continue $ unEitherSt st
    $ insertMembers_atFocus_puffer st
  B.EvKey (B.KChar 'c') [B.MMeta] -> B.continue
    $ closeSubviews_atFocus_puffer st
  B.EvKey (B.KChar 'b') [B.MMeta] -> B.continue
    $ unEitherSt st
    $ st & cons_focusedViewResult_asChild_inPuffer

  B.EvKey (B.KChar 'w') [B.MMeta] -> do
    -- TODO : slightly buggy: conjures, copies some empty lines.
    liftIO ( toClipboard $ unlines $ resultsText_puffer st )
    B.continue $ st
      & showReassurance "Results window copied to clipboard."

  B.EvKey (B.KChar 'e') [B.MMeta] -> B.continue
    $ moveFocusedRsltView_puffer DirPrev
    $ st & hideReassurance
  B.EvKey (B.KChar 'd') [B.MMeta] -> B.continue
    $ moveFocusedRsltView_puffer DirNext
    $ st & hideReassurance
  B.EvKey (B.KChar 'f') [B.MMeta] -> B.continue
    $ moveFocusedRsltView_puffer DirDown
    $ st & hideReassurance
  B.EvKey (B.KChar 's') [B.MMeta] -> B.continue
    $ moveFocusedRsltView_puffer DirUp
    $ st & hideReassurance

  _ -> handleUncaughtInput st ev

parseAndRunCommand_puffer :: St -> B.EventM BrickName (B.Next St)
parseAndRunCommand_puffer st =
  let cmd = unlines $ B.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left parseErr -> B.continue $ unEitherSt st $ Left parseErr
      -- PITFALL: these two Lefts have different types.
    Right parsedCmd -> case runParsedCommand_puffer parsedCmd st of
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
runParsedCommand_puffer ::
  Command -> St -> Either String (B.EventM BrickName (B.Next St))

runParsedCommand_puffer (CommandFind s h) st = do
  let r = st ^. appRslt
      title = "runParsedCommand, called on CommandFind"

  (as :: Set Addr)   <- prefixLeft title
    $ hExprToAddrs r (mempty :: Subst Addr) h

  let v = PTree { _pTreeLabel = VQuery s
                , _pTreeHasFocus = True
                , _pMTrees =
                  P.fromList $ map v_qr $ S.toList as
                } where

        v_qr :: Addr -> PTree RsltView
        v_qr a = pTreeLeaf $ let
              (rv :: Either String ViewResult) = resultView r a
              (err :: String -> ViewResult) = \se -> error ("runParsedCommand (Find): should be impossible: `a` should be present, as it was just found by `hExprToAddrs`, but here's the original error: " ++ se)
          in VResult $ either err id rv

  Right $ B.continue $ st & showingInMainWindow .~ Results
                          & stSetFocusedPuffer . pufferQuery .~ s
                          & stSetFocusedPuffer . pufferRsltViewTree .~ v

runParsedCommand_puffer (CommandInsert e) st =
  either Left (Right . f)
  $ exprToAddrInsert (st ^. appRslt) e
  where
    f :: (Rslt, Addr) -> B.EventM BrickName (B.Next St)
    f (r,a) = B.continue $ st & appRslt .~ r
              & showReassurance ("Expr added at Addr " ++ show a)
              & showingInMainWindow .~ Results

runParsedCommand_puffer (CommandLoad f) st = Right $ do
  (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
  if bad
    then B.continue $ st & showError ("Non-existent folder: " ++ f)
    else do r <- liftIO $ readRslt f
            B.continue $ st & appRslt .~ r
                            & showReassurance "Rslt loaded."
                            & showingInMainWindow .~ Results

runParsedCommand_puffer (CommandSave f) st = Right $ do
  (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
  st' <- if bad
    then return $ st & showError ("Non-existent folder: " ++ f)
    else do liftIO $ writeRslt f $ st ^. appRslt
            return $ st & showingInMainWindow .~ Results
                   & showReassurance "Rslt saved."
  B.continue st'
