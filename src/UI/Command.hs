{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.Command (
  parseAndRunCommand -- ^ St -> B.EventM BrickName (B.Next St)
  , runCommand -- ^ Command -> St
               -- -> Either String (B.EventM BrickName (B.Next St))
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Set (Set)
import qualified Data.Set                 as S
import qualified Data.Vector              as V
import           Lens.Micro
import           System.Directory

import qualified Brick.Main               as B
import qualified Brick.Types              as B
import qualified Brick.Widgets.Edit       as B

import Hash.HLookup
import Qseq.QTypes
import Rslt.Edit
import Rslt.Files
import Rslt.RTypes
import UI.IParse
import UI.ITypes
import UI.IUtil
import UI.Window
import UI.String
import Util.Misc
import Util.VTree


parseAndRunCommand :: St -> B.EventM BrickName (B.Next St)
parseAndRunCommand st =
  let cmd = unlines $ B.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left parseErr -> B.continue $ unEitherSt st $ Left parseErr
      -- PITFALL: these two Lefts have different types.
    Right parsedCmd -> case runCommand parsedCmd st of
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
runCommand ::
  Command -> St -> Either String (B.EventM BrickName (B.Next St))

runCommand (CommandFind s h) st = do
  let r = st ^. appRslt
      title = "runCommand, called on CommandFind"

  (as :: Set Addr)   <- prefixLeft title
    $ hExprToAddrs r (mempty :: Subst Addr) h

  let v = VTree { _vTreeFocalChild = 0
                , _vTreeIsFocused = False
                , _vTreeLabel = VQuery s
                , _vTrees =
                  V.fromList $ map v_qr $ S.toList as
                } where

        v_qr :: Addr -> VTree RsltView
        v_qr a = vTreeLeaf $ let
              (rv :: Either String ViewResult) = resultView r a
              (err :: String -> ViewResult) = \se -> error ("runCommand (Find): should be impossible: `a` should be present, as it was just found by `hExprToAddrs`, but here's the original error: " ++ se)
          in VResult $ either err id rv

  Right $ B.continue $ st & showingInMainWindow .~ Results
                          & stBuffer st . bufferQuery .~ s
                          & stBuffer st . bufferPath .~ []
                          & stBuffer st . bufferView .~ v

runCommand (CommandInsert e) st =
  either Left (Right . f)
  $ exprToAddrInsert (st ^. appRslt) e
  where
    f :: (Rslt, Addr) -> B.EventM BrickName (B.Next St)
    f (r,a) = B.continue $ st & appRslt .~ r
              & showReassurance ("Expr added at Addr " ++ show a)
              & showingInMainWindow .~ Results

runCommand (CommandLoad f) st = Right $ do
  (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
  if bad
    then B.continue $ st & showError ("Non-existent folder: " ++ f)
    else do r <- liftIO $ readRslt f
            B.continue $ st & appRslt .~ r
                            & showReassurance "Rslt loaded."
                            & showingInMainWindow .~ Results

runCommand (CommandSave f) st = Right $ do
  (bad :: Bool) <- liftIO $ not <$> doesDirectoryExist f
  st' <- if bad
    then return $ st & showError ("Non-existent folder: " ++ f)
    else do liftIO $ writeRslt f $ st ^. appRslt
            return $ st & showingInMainWindow .~ Results
                   & showReassurance "Rslt saved."
  B.continue st'
