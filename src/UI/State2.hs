{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.State2 where

import           Control.Monad.IO.Class (liftIO)
--import           Data.Functor.Foldable
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Zipper.Generic as TxZ
--import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Lens.Micro

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
import UI.ITypes2
import Util.Misc


initialState2 :: Rslt -> St2
initialState2 r = St2 {
    _st2_focusRing = B.focusRing [Commands, Results]
      -- Almost always (for safety), Results is listed first. Not so
      -- here, because we want focus to start on the Commands window.
  , _st2_view  = View { _viewPath = []
                      , _viewFocus = 0
                      , _viewContent = Left ""
                      , _viewSubviews = V.empty
                      }
  , _st2_focusedSubview = []
  , _st2_uiError   = ""
  , _st2_commands  = B.editor Commands Nothing ""
  , _st2_appRslt   = r
  , _st2_shownInResultsWindow = ShowingResults
  }


resultsText2 :: St2 -> [String]
resultsText2 st = f 0 $ st ^. st2_view where
  indent :: Int -> String -> String
  indent i s = replicate (2*i) ' ' ++ s

  f :: Int -> View -> [String]
  f i v = indent i (vShow $ v ^. viewContent)
    : concatMap (f $ i+1) (V.toList $ v ^. viewSubviews)


vShow :: Either ViewQuery ViewResult -> String
vShow (Left vq)  = vq
vShow (Right qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)


emptyCommandWindow2 :: St2 -> St2
emptyCommandWindow2 = st2_commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing


parseAndRunCommand2 :: St2 -> B.EventM WindowName (B.Next St2)
parseAndRunCommand2 st =
  let cmd = unlines $ B.getEditContents $ st ^. st2_commands
  in case pCommand (st ^. st2_appRslt) cmd of
    Left s1 -> B.continue
     $ st2_shownInResultsWindow .~ ShowingError
     $ st2_uiError .~ s1
     $ st
    Right c -> case runCommand2 c st of
      Left s2 -> B.continue
        $ st2_shownInResultsWindow .~ ShowingError
        $ st2_uiError .~ s2
        $ st
      Right evNextSt -> evNextSt


runCommand2 ::
  Command -> St2 -> Either String (B.EventM WindowName (B.Next St2))

runCommand2 (CommandFind s h) st = do
  let r = st ^. st2_appRslt
      title = "runCommand, called on CommandFind"

  (as :: Set Addr)   <- prefixLeft title
    $ hExprToAddrs r (mempty :: Subst Addr) h
  (es :: Map Addr Expr)   <- ifLefts_map title
    $ M.fromSet (addrToExpr r) as
  (ss :: Map Addr String) <- ifLefts_map title
    $ M.map (eShow r) es

  let qr :: Addr -> ViewResult
      qr a = ViewResult { _viewResultAddr = a
                        , _viewResultExpr = (M.!) es a
                        , _viewResultString = (M.!) ss a }
      v_qr :: Addr -> View
      v_qr a = View { _viewPath = [SvQuery s]
                    , _viewFocus = 0
                    , _viewContent = Right $ qr a
                    , _viewSubviews = V.empty }
      v = View {
        _viewPath = []
        , _viewFocus = 0
        , _viewContent = Left s
        , _viewSubviews = V.fromList $ map v_qr $ S.toList as
        }

  Right $ B.continue $ st
    & st2_focusedSubview .~ [SvQuery s]
    & st2_view .~ v
    & st2_shownInResultsWindow .~ ShowingResults

runCommand2 (CommandInsert e) st =
  either Left (Right . f) $ exprToAddrInsert (st ^. st2_appRslt) e
  where f :: (Rslt, Addr) -> B.EventM WindowName (B.Next St2)
        f (r,_) = B.continue
          $ st2_appRslt .~ r
          $ st2_shownInResultsWindow .~ ShowingResults
          $ st

runCommand2 (CommandLoad f) st =
  Right $ do r <- liftIO $ readRslt f
             B.continue $ st & st2_appRslt .~ r

runCommand2 (CommandSave f) st =
  Right ( liftIO ( writeRslt f $ st ^. st2_appRslt )
          >> B.continue st )
