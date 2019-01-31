{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt.Files where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.RTypes
import Util


write :: FilePath -> Rslt -> IO ()
write p r = let
  writeExpr :: (Addr, Expr) -> IO ()
  writeExpr (a,e) =
    writeFile name $ show e
    where name = p ++ "/" ++ show a ++ ".rslt"
  in mapM_ writeExpr $ M.toList $ _exprAt r
