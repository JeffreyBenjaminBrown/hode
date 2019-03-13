{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.State where

import           Control.Monad.IO.Class (liftIO)
import           Data.Map (Map)
import qualified Data.Map                 as M
import           Data.Set (Set)
import qualified Data.Set                 as S
import qualified Data.Text.Zipper.Generic as TxZ
import qualified Data.Vector              as V
import           Lens.Micro

import qualified Brick.Main               as B
import qualified Brick.Focus              as B
import qualified Brick.Types              as B
import qualified Brick.Widgets.Edit       as B

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
      -- Almost always (for safety), Results is listed first. Not so
      -- here, because we want focus to start on the Commands window.
  , _view  = View { _viewFocus = 0
                      , _viewIsFocused = False
                      , _viewContent = Left ""
                      , _viewSubviews = V.empty
                      }
  , _pathToFocus = []
  , _uiError   = ""
  , _commands  = B.editor Commands Nothing ""
  , _appRslt   = r
  , _shownInResultsWindow = ShowingResults
  }


resultsText :: St -> [String]
resultsText st = f 0 $ st ^. view where
  indent :: Int -> String -> String
  indent i s = replicate (2*i) ' ' ++ s

  f :: Int -> View -> [String]
  f i v = indent i (vShow $ v ^. viewContent)
    : concatMap (f $ i+1) (V.toList $ v ^. viewSubviews)


vShow :: Either ViewQuery ViewResult -> String
vShow (Left vq)  = vq
vShow (Right qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)


emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing


parseAndRunCommand :: St -> B.EventM WindowName (B.Next St)
parseAndRunCommand st =
  let cmd = unlines $ B.getEditContents $ st ^. commands
  in case pCommand (st ^. appRslt) cmd of
    Left s1 -> B.continue
     $ shownInResultsWindow .~ ShowingError
     $ uiError .~ s1
     $ st
    Right c -> case runCommand c st of
      Left s2 -> B.continue
        $ shownInResultsWindow .~ ShowingError
        $ uiError .~ s2
        $ st
      Right evNextSt -> evNextSt


runCommand ::
  Command -> St -> Either String (B.EventM WindowName (B.Next St))

runCommand (CommandFind s h) st = do
  let r = st ^. appRslt
      title = "runCommand, called on CommandFind"

  (as :: Set Addr)   <- prefixLeft title
    $ hExprToAddrs r (mempty :: Subst Addr) h
  (es :: Map Addr Expr)   <- ifLefts_map title
    $ M.fromSet (addrToExpr r) as
  (ss :: Map Addr String) <- ifLefts_map title
    $ M.map (eShow r) es

  let v = View { _viewFocus = 0
               , _viewIsFocused = False
               , _viewContent = Left s
               , _viewSubviews =
                 V.fromList $ map v_qr $ S.toList as
               } where
        v_qr :: Addr -> View
        v_qr a = View { _viewFocus = 0
                      , _viewIsFocused = False
                      , _viewContent = Right $ qr
                      , _viewSubviews = V.empty } where
          qr :: ViewResult
          qr = ViewResult { _viewResultAddr = a
                          , _viewResultExpr = (M.!) es a
                          , _viewResultString = (M.!) ss a }

  Right $ B.continue $ st
    & pathToFocus .~ []
    & view .~ v
    & shownInResultsWindow .~ ShowingResults

runCommand (CommandInsert e) st =
  either Left (Right . f) $ exprToAddrInsert (st ^. appRslt) e
  where f :: (Rslt, Addr) -> B.EventM WindowName (B.Next St)
        f (r,_) = B.continue
          $ appRslt .~ r
          $ shownInResultsWindow .~ ShowingResults
          $ st

runCommand (CommandLoad f) st =
  Right $ do r <- liftIO $ readRslt f
             B.continue $ st & appRslt .~ r

runCommand (CommandSave f) st =
  Right ( liftIO ( writeRslt f $ st ^. appRslt )
          >> B.continue st )
