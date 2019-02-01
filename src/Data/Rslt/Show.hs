{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt.Show where

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
import Data.Rslt.Lookup
import Util


-- | TODO : This strategy is bad. Instead of showing an Expr,
-- show an ExprImg. Those are something the depth of which is easy
-- to compute.
eShow :: Rslt -> Expr -> Either String String
eShow _ (Word s) = Right s

eShow r (Tplt as) = do
  es <- ifLefts "eShow" $ map (exprAt r) as
  ss <- ifLefts "eShow" $ map (eShow r) es
  Right $ concat $ L.intersperse " _ " ss

eShow r rel@(Rel memAs tpltA) = do
  (memEs :: [Expr]) <- ifLefts "eShow" $ map (exprAt r) memAs
  memSs <- ifLefts "eShow" $ map (eShow r) memEs
  tpltE <- prefixLeft "eShow" $ exprAt r tpltA
  case tpltE of

    Tplt jointAs -> do
      jointEs <- ifLefts "eShow" $ map (exprAt r) jointAs
      jointSs <- ifLefts "eShow" $ map (eShow r) jointEs
      Right $ concat $ map (\(j,m) -> j ++ " " ++ m)
        $ zip jointSs memSs
    _ -> Left $ "eShow: in " ++ show rel ++ ", " ++ show tpltA
         ++ " is not the Addr of a Tplt.\n"
