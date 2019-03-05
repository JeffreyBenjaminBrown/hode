{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.State where

import           Control.Monad.IO.Class (liftIO)
import           Lens.Micro
import           Data.Set (Set)
import           Data.Map (Map)
import qualified Data.Map as M
--import           Data.Vector (Vector)
import qualified Data.Vector as V

--import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z

import qualified Brick.Main as B
import qualified Brick.Focus as B
import qualified Brick.Types as B
import qualified Brick.Widgets.Edit as B

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
    _focusRing = B.focusRing [Commands, Results]
      -- Almost always (for safety), Results is listed first. Not so here,
      -- because we want focus to start on the Commands window.
  , _results   = B.editor Results Nothing "" -- Maybe : line number limit
  , _results'  = VQuery { _vQueryString = ""
                        , _vQueryResults = M.empty }
  , _uiError   = ""
  , _commands  = B.editor Commands Nothing ""
  , _appRslt   = r
  , _showingThing = ShowingResults
  }

focusedWindow :: St -> B.Editor String Name
focusedWindow st = let
  err = error "focusedWindow: impossible."
  f = \case Results -> st ^. results
            Commands -> st ^. commands
  in maybe err f
     $ B.focusGetCurrent
     $ st ^. focusRing

editor_replaceText ::
  Lens' St (B.Editor String Name) -> [String] -> (St -> St)
editor_replaceText windowGetter ss =
  windowGetter . B.editContentsL .~ Z.textZipper ss Nothing

parseAndRunCommand :: St -> B.EventM Name (B.Next St)
parseAndRunCommand st =
  let cmd = unlines $ B.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left s1  -> B.continue -- editor_replaceText results (lines s1) st
     $ showingThing .~ ShowingError
     $ uiError .~ s1
     $ st
    Right c -> case runCommand c st of
      Left s2 -> B.continue -- editor_replaceText results (lines s2) st
        $ showingThing .~ ShowingError
        $ uiError .~ s2
        $ st
      Right evNextSt -> evNextSt

runCommand :: Command -> St -> Either String (B.EventM Name (B.Next St))
runCommand (CommandInsert e) st =
  either Left (Right . f) $ exprToAddrInsert (st ^. appRslt) e
  where f :: (Rslt, Addr) -> B.EventM Name (B.Next St)
        f (r,_) = B.continue
          $ appRslt .~ r
          $ showingThing .~ ShowingResults
          $ st

runCommand (CommandFind s h) st = do
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

  let qr (a :: Addr) = QueryResult { _resultExpr = (M.!) es a
                                   , _resultString = (M.!) ss a
                                   , _subQueries = V.empty }
      vq = VQuery { _vQueryString = s
                  , _vQueryResults = let
                      f :: Addr -> a -> QueryResult
                      f k _ = qr k
                    in M.mapWithKey f es }

  Right $ B.continue
    $ editor_replaceText results ss1
    $ results' .~ vq
    $ showingThing .~ ShowingResults
    $ st

runCommand (CommandLoad f) st =
  Right $ do r <- liftIO $ readRslt f
             B.continue $ st & appRslt .~ r

runCommand (CommandSave f) st =
  Right ( liftIO ( writeRslt f $ st ^. appRslt )
          >> B.continue st )
