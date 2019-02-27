{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Files where

import qualified Data.Map       as M
import           System.Directory (listDirectory)
import           System.FilePath.Posix (dropExtension, takeExtension)

import Rslt.RTypes
import Rslt.Index


-- | PITFALL: Tested only by hand:
-- > import qualified Test.Rslt.RData as D
-- > writeRslt "test-io" D.rslt
-- > x <- readRslt "test-io"
-- > x == D.rslt

readRslt :: FilePath -> IO (Rslt)
readRslt p0 = do
  files <- filter (\f -> takeExtension f == ".rslt")
           <$> listDirectory p0
  (es :: [(Addr, RefExpr)]) <- let
      f p = do (e :: RefExpr) <- read <$> readFile (p0 ++ "/" ++ p)
               let (a :: Addr) = read $ dropExtension p
               return (a,e)
      in mapM f files
  return $ mkRslt $ M.fromList es

writeRslt :: FilePath -> Rslt -> IO ()
writeRslt p r = let
  writeRefExpr :: (Addr, RefExpr) -> IO ()
  writeRefExpr (a,e) =
    writeFile name $ show e ++ "\n"
    where name = p ++ "/" ++ show a ++ ".rslt"
  in mapM_ writeRefExpr $ M.toList $ _addrToRefExpr r
