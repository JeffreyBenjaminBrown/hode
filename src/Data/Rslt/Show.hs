{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rslt.Show where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Data.Text (strip, pack, unpack)
import           System.Directory (listDirectory)
import           System.FilePath.Posix (dropExtension, takeExtension)

import Data.Rslt.RTypes
import Data.Rslt.Lookup
import Util


-- https://unicode-search.net/unicode-namesearch.pl?term=bracket
bracket_angle_big_left = '⦑' -- C-x 8 Ret 2991
bracket_angle_big_right = '⦒' -- C-x 8 Ret 2992
bracket_angle_small_left = '«' -- C-x 8 <
bracket_angle_small_right = '»' -- C-x 8 >


depth :: ImgOfExpr -> Int
depth (ImgOfWord _) = 0
depth (ImgOfAddr _) = 0
depth (ImgOfRel mems _) = 1 + maximum (map depth mems)
depth (ImgOfTplt mems) = 0 -- ^ TODO ? consider Tplts with non-Word members
depth (ImgOfPar sis _) = 1 + maximum (map (depth . snd) sis)


hashUnlessEmptyStartOrEnd :: Int -> [String] -> [String]
hashUnlessEmptyStartOrEnd k joints = case joints of
  [] -> []
  s : ss ->   hashUnlessEmpty    k s
            : hashUnlessEmptyEnd k ss

  where
  hash :: Int -> String -> String
  hash k s = replicate k '#' ++ s

  hashUnlessEmpty :: Int -> String -> String
  hashUnlessEmpty _ "" = ""
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [String] -> [String]
  hashUnlessEmptyEnd k [] = []
  hashUnlessEmptyEnd k [s] =       [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss


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


eShow :: Rslt -> ImgOfExpr -> Either String String
eShow r (ImgOfAddr a) = do
  e <- exprAt r a
  case e of
    Word w    ->    eShow r $ ImgOfWord w
    Rel ms t  ->    eShow r $ ImgOfRel (map ImgOfAddr ms) $ ImgOfAddr t
    Tplt js   ->    eShow r $ ImgOfTplt $ map ImgOfAddr js
    Par sas s -> let (ss, as) = unzip sas
                 in eShow r $ ImgOfPar (zip ss $ map ImgOfAddr as) s

eShow r (ImgOfWord w) = Right w

eShow r (ImgOfTplt js) = do
  ss <- ifLefts "eShow" $ map (eShow r) js
  Right $ concat $ L.intersperse " _ " ss

eShow r i@(ImgOfRel ms (ImgOfTplt js)) = do
  mss <-     ifLefts "eShow" $ map (eShow r) ms
  jss <- hashUnlessEmptyStartOrEnd (depth i)
         <$> ifLefts "eShow" ( map (eShow r) js )
  Right $ unpack . strip . pack $ concat
    $ map (\(m,j) -> m ++ " " ++ j ++ " ")
    $ zip ("" : mss) jss
eShow r i@(ImgOfRel ms (ImgOfAddr a)) = error "unfinished"
eShow r i@(ImgOfRel _ _) =
  Left $ "eShow: ImgOfRel with non-Tplt in Tplt position: " ++ show i

eShow r (ImgOfPar ps s) = do
  let (ss,ms) = unzip ps
  (mis :: [String]) <- ifLefts "eShow" $ map (eShow r) ms
  let showPair :: (String, String) -> String
      showPair (s,mi) = s ++ " " ++ [bracket_angle_big_left]
        ++ mi ++ [bracket_angle_big_right] ++ " "
  Right $ concat (map showPair $ zip ss mis) ++ " " ++ s
