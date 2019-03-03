{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.State where

import           Control.Monad.IO.Class (liftIO)
import           Lens.Micro
import           Data.Set (Set)
import           Data.Map (Map)
import qualified Data.Map as M

--import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z

import qualified Brick.Main as B
import qualified Brick.Focus as BF
import qualified Brick.Types as BT
import qualified Brick.Widgets.Edit as BE

import Hash.HLookup
import Qseq.QTypes
import Rslt.Edit
import Rslt.Files
import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.IParse
import UI.ITypes
import Util.Misc


initialState :: Rslt -> St
initialState r = St {
    _focusRing = BF.focusRing [Commands, Results]
      -- Almost always (for safety), Results is listed first,
      -- but we want focus to start on the Commands window.
  , _results   = BE.editor Results Nothing "" -- Maybe : line number limit
  , _commands  = BE.editor Commands Nothing ""
  , _appRslt   = r
  , _history   = []
  }


focusedWindow :: St -> BE.Editor String Name
focusedWindow st = let
  err = error "focusedWindow: impossible."
  f = \case Results -> st ^. results
            Commands -> st ^. commands
  in maybe err f
     $ BF.focusGetCurrent
     $ st ^. focusRing

editor_replaceText ::
  Lens' St (BE.Editor String Name) -> [String] -> (St -> St)
editor_replaceText windowGetter ss =
  windowGetter . BE.editContentsL .~ Z.textZipper ss Nothing

parseAndRunCommand :: St -> BT.EventM Name (BT.Next St)
parseAndRunCommand st =
  let cmd = unlines $ BE.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left s1  -> B.continue $ editor_replaceText results (lines s1) st
    Right c -> case runCommand c st of
      Left s2 -> B.continue $ editor_replaceText results (lines s2) st
      Right st' -> st'


runCommand :: Command -> St -> Either String (BT.EventM Name (BT.Next St))
runCommand (CommandInsert e) st =
  either Left (Right . f) $ exprToAddrInsert (st ^. appRslt) e
  where f :: (Rslt, Addr) -> BT.EventM Name (BT.Next St)
        f (r,_) = B.continue $ st & appRslt .~ r

runCommand (CommandFind h) st = do
  let r = st ^. appRslt
      title = "runCommand, called on CommandFind"

  (as :: Set Addr)   <- prefixLeft title
    $ hExprToAddrs r (mempty :: Subst Addr) h
  (es :: Map Addr Expr)   <- ifLefts_map title
    $ M.fromSet (addrToExpr r) as
  (ss :: Map Addr String) <- ifLefts_map title
    $ M.map (eShow r) es
  let (ss1 :: [String]) = map f $ M.toList ss where
        f (addr,expr) = show addr ++ ": " ++ expr
  Right $ B.continue
    $ editor_replaceText results ss1 st

runCommand (CommandLoad f) st =
  Right $ do r <- liftIO $ readRslt f
             B.continue $ st & appRslt .~ r

runCommand (CommandSave f) st =
  Right ( liftIO ( writeRslt f $ st ^. appRslt )
          >> B.continue st )
