-- | = Functions from an `Rslt` and a parsed `String`,
-- to search, insert, show.
-- Theoretically, one could maintain an Rslt using GHCI with just these,
-- without ever using the TUI.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.NoUI (
    nPExpr         -- ^         String -> Either String PExpr
  , nHExpr         -- ^ Rslt -> String -> Either String HExpr
  , nHExpr'        -- ^         String -> Either String HExpr
  , nExpr          -- ^ Rslt -> String -> Either String Expr
  , nExpr'         -- ^         String -> Either String Expr
  , nInsert        -- ^ Rslt -> String -> Either String (Rslt, Addr)
  , nInsert'       -- ^ Rslt -> String -> Either String Rslt
  , nInserts       -- ^ Foldable f =>
                   --   Rslt -> f String -> Either String Rslt
  , nFindAddrs     -- ^ Rslt -> String -> Either String (Set Addr)
  , nFind          -- ^ Rslt -> String -> Either String (Set Expr)
  , nFindStrings   -- ^ Rslt -> String -> Either String (Set String)
  , nFindStringsIO -- ^ Rslt -> String -> IO ()
  ) where

import           Control.Monad (foldM)
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


nPExpr ::  String -> Either String PExpr
nPExpr s = prefixLeft "nPExpr: " $
           mapLeft show $
           parse _pHashExpr "parse error: " s

nHExpr ::  Rslt -> String -> Either String HExpr
nHExpr r s = nPExpr s >>= pExprToHExpr r

nHExpr' ::  String -> Either String HExpr
nHExpr' = nHExpr $ mkRslt mempty

nExpr ::  Rslt -> String -> Either String Expr
nExpr r s = prefixLeft "nExpr: " $
            nHExpr r s >>= hExprToExpr r

nExpr' ::  String -> Either String Expr
nExpr' = nExpr $ mkRslt mempty

nInsert :: Rslt -> String -> Either String (Rslt, Addr)
nInsert r s = prefixLeft "nInsert: " $
              nExpr r s >>= exprToAddrInsert r

nInsert' :: Rslt -> String -> Either String Rslt
nInsert' r s = fst <$> nInsert r s

nInserts :: Foldable f
         => Rslt -> f String -> Either String Rslt
nInserts r ss = foldM nInsert' r ss

nFindAddrs :: Rslt -> String -> Either String (Set Addr)
nFindAddrs r s = prefixLeft "nFindAddrs: " $
                 nHExpr r s >>=
                 hExprToAddrs r (mempty :: Subst Addr)

nFind :: Rslt -> String -> Either String (Set Expr)
nFind r s = prefixLeft "nFind: " $
            nFindAddrs r s >>=
            ifLefts_set . S.map ( addrToExpr r )

nFindStrings :: Rslt -> String -> Either String (Set String)
nFindStrings r s = prefixLeft "-> nFindStrings" $
                   nFind r s >>=
                   ifLefts_set . S.map (eShow r)

nFindStringsIO :: Rslt -> String -> IO ()
nFindStringsIO r s =
  case (nFindStrings r s :: Either String (Set String))
  of Left err -> putStrLn err
     Right ss -> mapM_ putStrLn ss
