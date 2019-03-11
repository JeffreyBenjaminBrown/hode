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

import qualified Data.Text.Zipper.Generic as TxZ

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
  , _results  = VQuery { _vQueryPath = [SvQuery ""]
                       , _vQueryString = ""
                       , _vQueryResults = V.empty
                       , _focusedResult = 0
                       }
  , _pathToFocus = []
  , _uiError   = ""
  , _commands  = B.editor Commands Nothing ""
  , _appRslt   = r
  , _shownInResultsWindow = ShowingResults
  }

vqIsFocused :: St -> VQuery -> Bool
vqIsFocused st vq = (st ^. pathToFocus) ==
  (vq ^. vQueryPath) ++ [SvQuery $ vq ^. vQueryString]

qrIsFocused :: St -> QueryResult -> Bool
qrIsFocused st qr = (st ^. pathToFocus) ==
  (qr ^. resultPath) ++ [SvResult $ qr ^. resultAddr]

resultsText :: St -> [String]
resultsText st = showVq 0 $ st ^. results where

  indent :: Int -> String -> String
  indent i s = replicate (2*i) ' ' ++ s

  showVq :: Int -> VQuery -> [String]
  showVq i vq =
    indent i (vq ^. vQueryString)
    : concatMap (showQR $ i+2) (V.toList $ vq ^. vQueryResults)

  showQR :: Int -> QueryResult -> [String]
  showQR i qr = let a = qr ^. resultAddr
    in indent i (show a ++ ": " ++ show (qr ^. resultString))
       : concatMap (showVq $ i+2) (V.toList $ qr ^. subQueries)

emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing

parseAndRunCommand :: St -> B.EventM WindowName (B.Next St)
parseAndRunCommand st =
  let cmd = unlines $ B.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left s1  -> B.continue -- editor_replaceText results (lines s1) st
     $ shownInResultsWindow .~ ShowingError
     $ uiError .~ s1
     $ st
    Right c -> case runCommand c st of
      Left s2 -> B.continue -- editor_replaceText results (lines s2) st
        $ shownInResultsWindow .~ ShowingError
        $ uiError .~ s2
        $ st
      Right evNextSt -> evNextSt

runCommand ::
  Command -> St -> Either String (B.EventM WindowName (B.Next St))
runCommand (CommandInsert e) st =
  either Left (Right . f) $ exprToAddrInsert (st ^. appRslt) e
  where f :: (Rslt, Addr) -> B.EventM WindowName (B.Next St)
        f (r,_) = B.continue
          $ appRslt .~ r
          $ shownInResultsWindow .~ ShowingResults
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

  let vq = VQuery {
          _vQueryPath = []
        , _vQueryString = s
        , _vQueryResults = let
            f addr _ = qr addr where
               qr a = QueryResult {
                         _resultPath = [SvQuery s]
                       , _resultAddr = a
                       , _resultExpr = (M.!) es a
                       , _resultString = (M.!) ss a
                       , _subQueries = V.empty
                       , _focusedSubQuery = 0
                       }
            in V.fromList $ M.elems $ M.mapWithKey f es
        , _focusedResult = 0
        }

  Right $ B.continue $ st
    & pathToFocus .~ [SvQuery s]
    & results .~ vq
    & shownInResultsWindow .~ ShowingResults

runCommand (CommandLoad f) st =
  Right $ do r <- liftIO $ readRslt f
             B.continue $ st & appRslt .~ r

runCommand (CommandSave f) st =
  Right ( liftIO ( writeRslt f $ st ^. appRslt )
          >> B.continue st )
