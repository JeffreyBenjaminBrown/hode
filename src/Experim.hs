{-# LANGUAGE ScopedTypeVariables #-}

module Experim where

import Text.Megaparsec

import Hash.Convert
import Hash.HLookup
import Hash.HParse
import Hash.HTypes
import Rslt.Edit
import Rslt.Index
import Rslt.RTypes
import Util.Misc


expr :: Rslt -> String -> Either String Expr
expr r s = do
  (pr :: PRel) <- mapLeft show $ parse pRel "expr" s
  pRelToHExpr pr >>= hExprToExpr r

x :: Either String (Rslt, Addr)
x = do
  let r = mkRslt mempty
  p <- mapLeft show $ parse pRel "doh!" "a"
  h <- pRelToHExpr p
  e <- hExprToExpr r h
  exprToAddrInsert r e
