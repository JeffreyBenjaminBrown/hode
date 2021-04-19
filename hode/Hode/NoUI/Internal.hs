module Hode.NoUI.Internal (
    nPExpr         -- ^         String -> Either String PExpr
  , nHExpr         -- ^ Rslt -> String -> Either String HExpr
  , nHExpr'        -- ^         String -> Either String HExpr
  , nExpr          -- ^ Rslt -> String -> Either String Expr
  , nExpr'         -- ^         String -> Either String Expr

  , nFindAddrs     -- ^ Rslt -> String -> Either String (Set Addr)
  , nFind_1_addr   -- ^ Rslt -> String -> Addr
  , nFindExprs     -- ^ Rslt -> String -> Either String (Set Expr)
  ) where

import           Data.Either.Combinators (mapLeft)
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import Hode.Hash.MkHExpr
import Hode.Hash.Lookup
import Hode.Hash.Parse
import Hode.Hash.Types
import Hode.Qseq.Types
import Hode.Rslt.Lookup
import Hode.Rslt.Types
import Hode.Rslt.Index
import Hode.Util.Misc


nPExpr ::  String -> Either String PExpr
nPExpr s = prefixLeft "nPExpr:" $
           mapLeft show $
           parse _pHashExpr "parse error: " s

nHExpr ::  Rslt -> String -> Either String HExpr
nHExpr r s = nPExpr s >>= pExprToHExpr r

nHExpr' ::  String -> Either String HExpr
nHExpr' = nHExpr $ mkRslt mempty

nExpr ::  Rslt -> String -> Either String Expr
nExpr r s = prefixLeft "nExpr:" $
            nHExpr r s >>= hExprToExpr r

nExpr' ::  String -> Either String Expr
nExpr' = nExpr $ mkRslt mempty

nFindAddrs :: Rslt -> String -> Either String (Set Addr)
nFindAddrs r s = prefixLeft "nFindAddrs:" $
                 nHExpr r s >>=
                 hExprToAddrs r (mempty :: Subst Addr)

nFind_1_addr :: Rslt -> String -> Addr
nFind_1_addr r s =
  either (error "nFind_1_addr: not found?") id $
  head . S.toList <$>
  nFindAddrs r s

nFindExprs :: Rslt -> String -> Either String (Set Expr)
nFindExprs r s = prefixLeft "nFind:" $
                 nFindAddrs r s >>=
                 ifLefts_set . S.map ( addrToExpr r )
