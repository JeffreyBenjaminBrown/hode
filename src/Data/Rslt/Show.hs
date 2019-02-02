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
bracket_angle_big_left    = '⦑' -- C-x 8 Ret 2991
bracket_angle_big_right   = '⦒' -- C-x 8 Ret 2992
bracket_angle_small_left  = '«' -- C-x 8 <
bracket_angle_small_right = '»' -- C-x 8 >


depth :: Expr -> Int
depth (Word _)     = 0
depth (ExprAddr _) = 0
depth (Rel mems _) = 1 + maximum (map depth mems)
depth (Tplt mems)  = 0 -- ^ TODO ? consider Tplts with non-Word members
depth (Par sis _)  = 1 + maximum (map (depth . snd) sis)


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
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss


imgOfExpr :: Rslt -> RefExpr -> Either String Expr
imgOfExpr _ (Word' w) = Right $ Word w
imgOfExpr r (Tplt' jointAs) = do
  (jointEs  :: [RefExpr])   <- ifLefts "imgOfExpr" $ map (refExprAt r) jointAs
  (jointEis :: [Expr]) <- ifLefts "imgOfExpr" $ map (imgOfExpr r) jointEs
  Right $ Tplt jointEis

imgOfExpr r (Rel' memAs tA) = do
  (memEs  :: [RefExpr])   <- ifLefts    "imgOfExpr" $ map (refExprAt r) memAs
  (memEis :: [Expr]) <- ifLefts    "imgOfExpr" $ map (imgOfExpr r) memEs
  (tE     :: RefExpr)     <- prefixLeft "imgOfExpr" $ refExprAt r tA
  (tEi    :: Expr)   <- prefixLeft "imgOfExpr" $ imgOfExpr r tE
  Right $ Rel memEis tEi

imgOfExpr r (Par' sas s) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [RefExpr])   <- ifLefts "imgOfExpr" $ map (refExprAt r) as
  (eis :: [Expr]) <- ifLefts "imgOfExpr" $ map (imgOfExpr r) es
  Right $ Par (zip ss eis) s


eShow :: Rslt -> Expr -> Either String String
eShow r (ExprAddr a) = do
  e <- refExprAt r a
  case e of
    Word' w    ->    eShow r $ Word w
    Tplt' js   ->    eShow r $ Tplt $ map ExprAddr js
    Rel' ms t  ->    eShow r $ Rel (map ExprAddr ms) $ ExprAddr t
    Par' sas s -> let (ss, as) = unzip sas
                  in eShow r $ Par (zip ss $ map ExprAddr as) s

eShow r (Word w) = Right w

eShow r (Tplt js) = do
  ss <- ifLefts "eShow" $ map (eShow r) js
  Right $ concat $ L.intersperse " _ " ss

eShow r i@(Rel ms (Tplt js)) = do
  mss <- ifLefts     "eShow" $ map (eShow r) ms
  jss <- hashUnlessEmptyStartOrEnd (depth i)
         <$> ifLefts "eShow" ( map (eShow r) js )
  Right $ unpack . strip . pack $ concat
    $ map (\(m,j) -> m ++ " " ++ j ++ " ")
    $ zip ("" : mss) jss

eShow r (Rel ms (ExprAddr a)) = do
  (te :: RefExpr)   <- prefixLeft "eShow" $ refExprAt r a
  (ti :: Expr) <- prefixLeft "eShow" $ imgOfExpr r te
  eShow r $ Rel ms ti
eShow r i@(Rel _ _) =
  Left $ "eShow: Rel with non-Tplt in Tplt position: " ++ show i

eShow r (Par ps s) = do
  let (ss,ms) = unzip ps
  (mis :: [String]) <- ifLefts "eShow" $ map (eShow r) ms
  let showPair :: (String, String) -> String
      showPair (s,mi) = s ++ " " ++ [bracket_angle_big_left]
        ++ mi ++ [bracket_angle_big_right] ++ " "
  Right $ concat (map showPair $ zip ss mis) ++ " " ++ s
