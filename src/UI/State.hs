{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.State where

import           Lens.Micro
import           Data.Set (Set)
import qualified Data.Set as S

--import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Hash.HLookup
import Qseq.QTypes
import Rslt.Edit
import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc


-- | = functions involving St

initialState :: Rslt -> St
initialState r = St {
    _focusRing = F.focusRing [Results, Commands]
  , _results   = E.editor Results Nothing "" -- Maybe : line number limit
  , _commands  = E.editor Commands Nothing ""
  , _appRslt   = r
  , _history   = []
  }

editor_replaceText ::
  Lens' St (E.Editor String Name) -> [String] -> (St -> St)
editor_replaceText windowGetter ss =
  windowGetter . E.editContentsL .~ Z.textZipper ss Nothing

runCommand :: Command -> St -> Either String St
runCommand (CommandInsert e) st =
  either Left (Right . f) $ exprToAddrInsert (st ^. appRslt) e
  where f :: (Rslt, Addr) -> St
        f (r,_) = st & appRslt .~ r

runCommand (CommandFind h) st = do
  let r = st ^. appRslt
      title = "runCommand, called on CommandFind"
  (as :: Set Addr)   <- prefixLeft title
                        $ hExprToAddrs r (mempty :: Subst Addr) h
  (es :: Set Expr)   <- ifLefts_set title
                        $ S.map ( addrToExpr r ) as
  (ss :: Set String) <- ifLefts_set title
                        $ S.map (eShow r) es
  Right $ editor_replaceText results (S.toList ss) st

--runCommand (CommandLoad f) st = do
--  writeRslt f (st

--writeRslt "test-io" D.rslt
--x <- readRslt "test-io"
--x == D.rslt
