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


depth :: ImgOfExpr -> Int
depth (ImgOfWord _) = 0
depth (ImgOfAddr _) = 0
depth (ImgOfRel mems _) = 1 + maximum (map depth mems)
depth (ImgOfTplt mems) = 0 -- ^ TODO ? consider Tplts with non-Word members
depth (ImgOfPar sis _) = 1 + maximum (map (depth . snd) sis)


imgOfExpr :: Rslt -> Expr -> Either String ImgOfExpr
imgOfExpr _ (Word w) = Right $ ImgOfWord w
imgOfExpr r (Tplt jointAs) = do
  (jointEs  :: [Expr])      <- ifLefts "imgOfExpr" $ map (exprAt r) jointAs
  (jointEis :: [ImgOfExpr]) <- ifLefts "imgOfExpr" $ map (imgOfExpr r) jointEs
  Right $ ImgOfTplt jointEis

imgOfExpr r (Rel memAs tA) = do
  (memEs  :: [Expr])      <- ifLefts    "imgOfExpr" $ map (exprAt r) memAs
  (memEis :: [ImgOfExpr]) <- ifLefts    "imgOfExpr" $ map (imgOfExpr r) memEs
  (tE     :: Expr)        <- prefixLeft "imgOfExpr" $ exprAt r tA
  (tEi    :: ImgOfExpr)   <- prefixLeft "imgOfExpr" $ imgOfExpr r tE
  Right $ ImgOfRel memEis tEi

imgOfExpr r (Par sas s) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [Expr])      <- ifLefts "imgOfExpr" $ map (exprAt r) as
  (eis :: [ImgOfExpr]) <- ifLefts "imgOfExpr" $ map (imgOfExpr r) es
  Right $ ImgOfPar (zip ss eis) s


--eShow :: Rslt -> ImgOfExpr -> Either String String
--eShow r 
