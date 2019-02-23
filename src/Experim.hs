{-# LANGUAGE ScopedTypeVariables #-}

module Experim where

import           Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec

import Hash.Convert
import Hash.HLookup
import Hash.HParse
--import Hash.HTypes
import Qseq.QTypes
import Rslt.Edit
import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import Util.Misc


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
pFindStringsIO r s = do
  let (e :: Either String (Set String)) =
        pFindStrings r s
  case e of
    Left err -> putStrLn err
    Right ss -> mapM_ putStrLn ss

-- import qualified Test.Rslt.RData as D
-- pFindStringsIO D.b2 "/hash _ #like _"
