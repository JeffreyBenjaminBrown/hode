-- | = Functions from an `Rslt` and a parsed `String`,
-- to search, insert, show.
-- Theoretically, one could maintain an Rslt using GHCI with just these,
-- without ever using the TUI.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.NoUI where

import           Data.Either.Combinators (mapLeft)
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.Megaparsec

import Hode.Hash.Convert
import Hode.Hash.HLookup
import Hode.Hash.HParse
import Hode.Hash.HTypes
import Hode.Qseq.QTypes
import Hode.Rslt.Edit
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.Rslt.Index
import Hode.Util.Misc


pHExpr ::  Rslt -> String -> Either String HExpr
pHExpr r s =
  mapLeft show (parse _pHashExpr "parse error" s) >>=
  pExprToHExpr r

nExpr ::  Rslt -> String -> Either String HExpr
nExpr r s =
  mapLeft show (parse _pHashExpr "parse error" s) >>=
  pExprToHExpr r

pExpr' ::  String -> Either String HExpr
pExpr' s =
  mapLeft show (parse _pHashExpr "parse error" s) >>=
  pExprToHExpr (mkRslt mempty)

pInsert :: Rslt -> String -> Either String (Rslt, Addr)
pInsert r s = prefixLeft "-> pInsert"
  $ mapLeft show (parse _pHashExpr "doh!" s)
  >>= pExprToHExpr r
  >>= hExprToExpr r
  >>= exprToAddrInsert r

pInsert' :: Rslt -> String -> Either String Rslt
pInsert' r s = fst <$> pInsert r s

pFind :: Rslt -> String -> Either String (S.Set Expr)
pFind r s = pHExpr r s >>=
           hExprToAddrs r mempty >>=
           ifLefts_set . S.map (addrToExpr r)

pFindAddrs :: Rslt -> String -> Either String (Set Addr)
pFindAddrs r s = prefixLeft "-> pFindAddrs"
  $ mapLeft show (parse pPExpr "doh!" s)
  >>= pExprToHExpr r
  >>= hExprToAddrs r (mempty :: Subst Addr)

pFindStrings :: Rslt -> String -> Either String (Set String)
pFindStrings r s = prefixLeft "-> pFindExprs" $ do
  (as :: Set Addr)   <- pFindAddrs r s
  (es :: Set Expr)   <- ifLefts_set $ S.map ( addrToExpr r ) as
  (ss :: Set String) <- ifLefts_set $ S.map (eShow r) es
  return ss

pFindStringsIO :: Rslt -> String -> IO ()
pFindStringsIO r s =
  case (pFindStrings r s :: Either String (Set String))
  of Left err -> putStrLn err
     Right ss -> mapM_ putStrLn ss
