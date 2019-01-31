{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt.Files where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           System.Directory (listDirectory)
import           System.FilePath.Posix (dropExtension, takeExtension)

import Data.Rslt.RTypes
import Data.Rslt.Index
import Util


readRslt :: FilePath -> IO (Rslt)
readRslt p0 = do
  files <- filter (\f -> takeExtension f == ".rslt")
           <$> listDirectory p0
  (es :: [(Addr, Expr)]) <- let
      f p = do (e :: Expr) <- read <$> readFile p
               let (a :: Addr) = read $ dropExtension p
               return (a,e)
      in mapM f files
  return $ mkRslt $ M.fromList es

writeRslt :: FilePath -> Rslt -> IO ()
writeRslt p r = let
  writeExpr :: (Addr, Expr) -> IO ()
  writeExpr (a,e) =
    writeFile name $ show e
    where name = p ++ "/" ++ show a ++ ".rslt"
  in mapM_ writeExpr $ M.toList $ _exprAt r
