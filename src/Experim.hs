{-# LANGUAGE ScopedTypeVariables #-}

module Experim where

import Text.Megaparsec

import Hash.Convert
import Hash.HLookup
import Hash.HParse
--import Hash.HTypes
import Rslt.Edit
--import Rslt.Index
import Rslt.RTypes
import Util.Misc


pInsert :: Rslt -> String -> Either String (Rslt, Addr)
pInsert r s = prefixLeft "pInsert"
  $ mapLeft show (parse pExpr "doh!" s)
  >>= pExprToHExpr
  >>= hExprToExpr r
  >>= exprToAddrInsert r
 
