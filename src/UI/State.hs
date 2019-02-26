{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.State where

import Lens.Micro
import Lens.Micro.TH

import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Hash.Convert
import Hash.HLookup
import Hash.HTypes
import Hash.HParse
import Qseq.QTypes
import Rslt.Edit
import Rslt.Index
import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import Util.Misc


-- | = Types

data Name = Results | Commands
  deriving (Ord, Show, Eq)

data St = St {
    _focusRing :: F.FocusRing Name
  , _results   :: E.Editor String Name
  , _commands  :: E.Editor String Name
  , _appRslt   :: Rslt
  , _history   :: [HExpr]
  }

makeLenses ''St


-- | = more

initialState :: Rslt -> St
initialState r = St {
    _focusRing = F.focusRing [Results, Commands]
  , _results   = E.editor Results Nothing "" -- Maybe : line number limit
  , _commands  = E.editor Commands Nothing ""
  , _appRslt   = r
  , _history   = []
  }


-- | = parse, search, insert, show

pInsert :: Rslt -> String -> Either String (Rslt, Addr)
pInsert r s = prefixLeft "pInsert"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToExpr r
  >>= exprToAddrInsert r

pFindAddrs :: Rslt -> String -> Either String (Set Addr)
pFindAddrs r s = prefixLeft "pFindAddrs"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToAddrs r (mempty :: Subst Addr)

pFindStrings :: Rslt -> String -> Either String (Set String)
pFindStrings r s = do
  (as :: Set Addr) <- prefixLeft "pFindExprs"
                      $ pFindAddrs r s
  (es :: Set Expr) <- ifLefts_set "pFindExprs"
                      $ S.map ( addrToExpr r ) as
  (ss :: Set String) <- ifLefts_set "pFindExprs"
                        $ S.map (eShow r) es
  return ss

pFindStringsIO :: Rslt -> String -> IO ()
pFindStringsIO r s =
  case (pFindStrings r s :: Either String (Set String))
  of Left err -> putStrLn err
     Right ss -> mapM_ putStrLn ss
