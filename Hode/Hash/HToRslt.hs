-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Hash.HToRslt where

import           Data.Either.Combinators (mapLeft)
import           Text.Megaparsec hiding (label)

import Hode.Rslt.RTypes
import Hode.Hash.HParse
import Hode.Hash.Convert
import Hode.Hash.HLookup
import Hode.Util.Misc
import Hode.Rslt.Edit
import Hode.Rslt.Index


pExpr :: String -> Either String Expr
pExpr s =
  mapLeft show (parse _pHashExpr "doh 1!" s)
  >>= pExprToHExpr emptyRslt
  >>= hExprToExpr emptyRslt

stringHExprsToRslt :: [String] -> Either String Rslt
stringHExprsToRslt ss = do
  es :: [Expr] <- ifLefts $ map pExpr ss
  fst <$> exprToAddrInsert_list emptyRslt es
