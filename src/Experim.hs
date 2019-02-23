{-# LANGUAGE ScopedTypeVariables #-}

module Experim where

import           Data.Set (Set)
--import qualified Data.Set as S
import Text.Megaparsec

import Hash.Convert
import Hash.HLookup
import Hash.HParse
import Hash.HTypes
import Qseq.QTypes
import Rslt.Edit
import Rslt.RTypes
import Util.Misc


pInsert :: Rslt -> String -> Either String (Rslt, Addr)
pInsert r s = prefixLeft "pInsert"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToExpr r
  >>= exprToAddrInsert r

-- Right p = pPExpr "/hash _ #like _"
-- bad: does not recognize the empty outer joints on the template,
-- at least if they are Any (_).
pPExpr :: String -> Either String PExpr
pPExpr s = prefixLeft "pShow"
  $ mapLeft show (parse pExpr "doh!" s)

-- Right h = pHExpr "/hash _ #like _"
pHExpr :: String -> Either String HExpr
pHExpr s = prefixLeft "pShow"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr

-- pFind D.b2 "/hash _ #like _"
-- import qualified Test.Rslt.RData as D
pFind :: Rslt -> String -> Either String (Set Addr)
pFind r s = prefixLeft "pShow"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToAddrs r (mempty :: Subst Addr)
