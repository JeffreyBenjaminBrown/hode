{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.VTree.Input (
  handleKeyboard_atResults             -- ^ St -> B.Event ->
                                       -- B.EventM BrickName (B.Next St)
  , handleKeyboard_atBufferWindow        -- ^ St -> B.Event ->
                                         -- B.EventM BrickName (B.Next St)

  , parseAndRunCommand        -- ^ St -> B.EventM BrickName (B.Next St)
  , runParsedCommand        -- ^ Command -> St ->
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
import UI.Input
import UI.ITypes
import UI.IUtil
import UI.Input.IParse
import UI.String
import UI.ViewExprNodeTree
import UI.VTree.String
import UI.VTree.ViewExprNodeTree
import UI.Window
import Util.Direction
import Util.Misc
import Util.PTree
import Util.VTree



handleKeyboard_atBufferWindow :: St -> B.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atBufferWindow st ev = case ev of
  B.EvKey (B.KChar 'e') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus_inBufferTree DirPrev
    $ st & hideReassurance
  B.EvKey (B.KChar 'd') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus_inBufferTree DirNext
    $ st & hideReassurance
  B.EvKey (B.KChar 'f') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus_inBufferTree DirDown
    $ st & hideReassurance
  B.EvKey (B.KChar 's') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocus_inBufferTree DirUp
    $ st & hideReassurance

  B.EvKey (B.KChar 'c') [B.MMeta] -> B.continue
    $ unEitherSt st . consBuffer_asChild emptySearchBuffer
    $ st & hideReassurance
  B.EvKey (B.KChar 't') [B.MMeta] -> B.continue
    $                 consBufferAtTop emptySearchBuffer
    $ st & hideReassurance

  _ -> handleUncaughtInput st ev

handleKeyboard_atResults :: St -> B.Event -> B.EventM BrickName (B.Next St)
handleKeyboard_atResults st ev = case ev of
  B.EvKey (B.KChar 'h') [B.MMeta] -> B.continue $ unEitherSt st
    $ insertHosts_atFocus   st
  B.EvKey (B.KChar 'm') [B.MMeta] -> B.continue $ unEitherSt st
    $ insertMembers_atFocus st
  B.EvKey (B.KChar 'c') [B.MMeta] -> B.continue $ unEitherSt st
    $ closeSubviews_atFocus st
  B.EvKey (B.KChar 'b') [B.MMeta] -> B.continue
    $ unEitherSt st
    $ st & cons_focusedViewExpr_asChild

  B.EvKey (B.KChar 'w') [B.MMeta] -> do
    -- TODO : slightly buggy: conjures, copies some empty lines.
    liftIO ( toClipboard $ unlines $ resultsText st )
    B.continue $ st
      & showReassurance "SearchBuffer window copied to clipboard."

  B.EvKey (B.KChar 'e') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocusedViewExprNode DirPrev
    $ st & hideReassurance
  B.EvKey (B.KChar 'd') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocusedViewExprNode DirNext
    $ st & hideReassurance
  B.EvKey (B.KChar 'f') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocusedViewExprNode DirDown
    $ st & hideReassurance
  B.EvKey (B.KChar 's') [B.MMeta] -> B.continue
    $ unEitherSt st . moveFocusedViewExprNode DirUp
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

runParsedCommand (CommandFind s h) st = do
  let r = st ^. appRslt
      title = "runParsedCommand, called on CommandFind"

  (as :: Set Addr)   <- prefixLeft title
    $ hExprToAddrs r (mempty :: Subst Addr) h

  let v = VTree { _vTreeFocalChild = 0
                , _vTreeIsFocused = False
                , _vTreeLabel = VQuery s
                , _vTrees =
                  V.fromList $ map v_qr $ S.toList as
                } where

        v_qr :: Addr -> VTree ViewExprNode
        v_qr a = vTreeLeaf $ let
              (rv :: Either String ViewExpr) = resultView r a
              (err :: String -> ViewExpr) = \se -> error ("runParsedCommand (Find): should be impossible: `a` should be present, as it was just found by `hExprToAddrs`, but here's the original error: " ++ se)
          in VenExpr $ either err id rv

  Right $ B.continue $ st & showingInMainWindow .~ SearchBuffer
                          & stBuffer st . bufferQuery .~ s
                          & stBuffer st . bufferPath .~ []
                          & stBuffer st . bufferView .~ v

runParsedCommand (CommandInsert e) st =
  either Left (Right . f)
  $ exprToAddrInsert (st ^. appRslt) e
  where
    f :: (Rslt, Addr) -> B.EventM BrickName (B.Next St)
    f (r,a) = B.continue $ st & appRslt .~ r
              & showReassurance ("Expr added at Addr " ++ show a)
              & showingInMainWindow .~ SearchBuffer

runParsedCommand (CommandLoad f) st = Right $ do
  (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
  if bad
    then B.continue $ st & showError ("Non-existent folder: " ++ f)
    else do r <- liftIO $ readRslt f
            B.continue $ st & appRslt .~ r
                            & showReassurance "Rslt loaded."
                            & showingInMainWindow .~ SearchBuffer

runParsedCommand (CommandSave f) st = Right $ do
  (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
  st' <- if bad
    then return $ st & showError ("Non-existent folder: " ++ f)
    else do liftIO $ writeRslt f $ st ^. appRslt
            return $ st & showingInMainWindow .~ SearchBuffer
                   & showReassurance "Rslt saved."
  B.continue st'
