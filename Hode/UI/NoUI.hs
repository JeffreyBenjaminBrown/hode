-- | = Functions from an `Rslt` and a parsed `String`,
-- to search, insert, show.
-- Theoretically, one could maintain an Rslt using GHCI with just these,
-- without ever using the TUI.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.NoUI (
    nHExpr         -- ^ Rslt -> String -> Either String HExpr
  , nHExpr'        -- ^         String -> Either String HExpr
  , nExpr          -- ^ Rslt -> String -> Either String Expr
  , nExpr'         -- ^         String -> Either String Expr
  , nInsert        -- ^ Rslt -> String -> Either String (Rslt, Addr)
  , nInsert'       -- ^ Rslt -> String -> Either String Rslt
  , nFindAddrs     -- ^ Rslt -> String -> Either String (Set Addr)
  , nFind          -- ^ Rslt -> String -> Either String (Set Expr)
  , nFindStrings   -- ^ Rslt -> String -> Either String (Set String)
  , nFindStringsIO -- ^ Rslt -> String -> IO ()
  ) where

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


nHExpr ::  Rslt -> String -> Either String HExpr
nHExpr r s =
  mapLeft show (parse _pHashExpr "parse error" s) >>=
  pExprToHExpr r

nHExpr' ::  String -> Either String HExpr
nHExpr' = nHExpr $ mkRslt mempty

nExpr ::  Rslt -> String -> Either String Expr
nExpr r s =
  mapLeft show (parse _pHashExpr "parse error" s) >>=
  pExprToHExpr r >>=
  hExprToExpr r

nExpr' ::  String -> Either String Expr
nExpr' = nExpr $ mkRslt mempty

nInsert :: Rslt -> String -> Either String (Rslt, Addr)
nInsert r s = prefixLeft "-> nInsert"
  $ mapLeft show (parse _pHashExpr "doh!" s)
  >>= pExprToHExpr r
  >>= hExprToExpr r
  >>= exprToAddrInsert r

nInsert' :: Rslt -> String -> Either String Rslt
nInsert' r s = fst <$> nInsert r s

nFindAddrs :: Rslt -> String -> Either String (Set Addr)
nFindAddrs r s = prefixLeft "-> nFindAddrs"
  $ mapLeft show (parse pPExpr "doh!" s)
  >>= pExprToHExpr r
  >>= hExprToAddrs r (mempty :: Subst Addr)

nFind :: Rslt -> String -> Either String (Set Expr)
nFind r s = prefixLeft "-> nFindExprs" $
  nFindAddrs r s >>=
  ifLefts_set . S.map ( addrToExpr r )

nFindStrings :: Rslt -> String -> Either String (Set String)
nFindStrings r s = prefixLeft "-> nFindExprs" $
  nFind r s >>=
  ifLefts_set . S.map (eShow r)

nFindStringsIO :: Rslt -> String -> IO ()
nFindStringsIO r s =
  case (nFindStrings r s :: Either String (Set String))
  of Left err -> putStrLn err
     Right ss -> mapM_ putStrLn ss
