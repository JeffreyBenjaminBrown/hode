{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Files where

import qualified Data.Map       as M
import           Text.Regex
import           System.Directory (listDirectory)
import           System.FilePath.Posix (dropExtension, takeExtension)

import Rslt.RTypes
import Rslt.Index


-- | PITFALL: Tested only by hand:
-- > import qualified Test.Rslt.RData as D
-- > writeRslt "test-io" D.rslt
-- > x <- readRslt "test-io"
-- > x == D.rslt

-- | PITFALL: `Rel`s are stored in a format that looks like it has an
-- unmatched trailing parenthesis, due to the use of _brief and _unbrief.

readRslt :: FilePath -> IO (Rslt)
readRslt p0 = do
  files <- filter (\f -> takeExtension f == ".rslt")
           <$> listDirectory p0
  (es :: [(Addr, RefExpr)]) <- let
      f p = do (e :: RefExpr) <-
                 read . _unBrief <$> readFile (p0 ++ "/" ++ p)
               let (a :: Addr) =
                     read $ dropExtension p
               return (a,e)
      in mapM f files
  return $ mkRslt $ M.fromList es

writeRslt :: FilePath -> Rslt -> IO ()
writeRslt p r = let
  writeRefExpr :: (Addr, RefExpr) -> IO ()
  writeRefExpr (a,e) =
    writeFile name $ _brief $ show e ++ "\n"
    where name = p ++ "/" ++ show a ++ ".rslt"
  in mapM_ writeRefExpr $ M.toList $ _addrToRefExpr r

_brief :: String -> String
_brief = subRegex_safe "^Phrase' "     "p " .
         subRegex_safe "^Tplt' "       "t " .
         subRegex_safe "^Rel' \\(Rel " "r "

_unBrief :: String -> String
_unBrief = subRegex_safe "^p " "Phrase' "   .
           subRegex_safe "^t " "Tplt' "     .
           subRegex_safe "^r " "Rel' (Rel "

subRegex_safe :: String -> String -> String -> String
subRegex_safe pat replacement input =
  let regex = mkRegex pat
  in case matchRegex regex input of
    Nothing -> input
    Just _  -> subRegex regex input replacement
