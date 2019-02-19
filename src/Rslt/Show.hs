{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Show where

import           Prelude hiding (lookup)
import qualified Data.List      as L
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Data.Text (strip, pack, unpack)

import Rslt.Lookup
import Rslt.RTypes
import Rslt.RUtil
import Util.Misc


-- https://unicode-search.net/unicode-namesearch.pl?term=bracket
bracket_angle_big_left, bracket_angle_big_right, bracket_angle_small_left, bracket_angle_small_right :: Char
bracket_angle_big_left    = '⦑' -- C-x 8 Ret 2991
bracket_angle_big_right   = '⦒' -- C-x 8 Ret 2992
bracket_angle_small_left  = '«' -- C-x 8 <
bracket_angle_small_right = '»' -- C-x 8 >


hashUnlessEmptyStartOrEnd :: Int -> [String] -> [String]
hashUnlessEmptyStartOrEnd k0 joints = case joints of
  [] -> []
  s : ss ->   hashUnlessEmpty    k0 s
            : hashUnlessEmptyEnd k0 ss

  where
  hash :: Int -> String -> String
  hash k s = replicate k '#' ++ s

  hashUnlessEmpty :: Int -> String -> String
  hashUnlessEmpty _ "" = ""
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [String] -> [String]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss


exprFromRefExpr :: Rslt -> RefExpr -> Either String Expr
exprFromRefExpr _ (Word' w) = Right $ Word w
exprFromRefExpr r (Tplt' jointAs) = do
  (jointEs  :: [RefExpr])   <-
    ifLefts "exprFromRefExpr" $ map (refExprAt r) jointAs
  (jointEis :: [Expr]) <-
    ifLefts "exprFromRefExpr" $ map (exprFromRefExpr r) jointEs
  Right $ Tplt jointEis

exprFromRefExpr r (Rel' memAs tA) = do
  (memEs  :: [RefExpr]) <- ifLefts    "exprFromRefExpr"
                          $ map (refExprAt r) memAs
  (memEis :: [Expr])    <- ifLefts    "exprFromRefExpr"
                           $ map (exprFromRefExpr r) memEs
  (tE     :: RefExpr)   <- prefixLeft "exprFromRefExpr"
                           $ refExprAt r tA
  (tEi    :: Expr)      <- prefixLeft "exprFromRefExpr"
                           $ exprFromRefExpr r tE
  Right $ Rel memEis tEi

exprFromRefExpr r (Par' sas s) = do
  let ((ss, as) :: ([String],[Addr])) = unzip sas
  (es  :: [RefExpr]) <- ifLefts "exprFromRefExpr" $ map (refExprAt r) as
  (eis :: [Expr])    <- ifLefts "exprFromRefExpr" $ map (exprFromRefExpr r) es
  Right $ Par (zip ss eis) s


eShow :: Rslt -> Expr -> Either String String
eShow r (Addr a) = do
  e <- refExprAt r a
  case e of
    Word' w    ->    eShow r $ Word w
    Tplt' js   ->    eShow r $ Tplt $ map Addr js
    Rel' ms t  ->    eShow r $ Rel (map Addr ms) $ Addr t
    Par' sas s -> let (ss, as) = unzip sas
                  in eShow r $ Par (zip ss $ map Addr as) s

eShow _ (Word w) = Right w

eShow r (Tplt js) = do
  ss <- ifLefts "eShow" $ map (eShow r) js
  Right $ concat $ L.intersperse " _ " ss

eShow r i@(Rel ms (Tplt js)) = do
  mss <-     ifLefts "eShow" $ map (eShow r) ms
  jss <- hashUnlessEmptyStartOrEnd (depth i)
         <$> ifLefts "eShow" ( map (eShow r) js )
  Right $ unpack . strip . pack $ concat
    $ map (\(m,j) -> m ++ " " ++ j ++ " ")
    $ zip ("" : mss) jss

eShow r (Rel ms (Addr a)) = do
  (te :: RefExpr) <- prefixLeft "eShow" $ refExprAt r a
  (ti :: Expr)    <- prefixLeft "eShow" $ exprFromRefExpr r te
  eShow r $ Rel ms ti
eShow _ i@(Rel _ _) =
  Left $ "eShow: Rel with non-Tplt in Tplt position: " ++ show i

eShow r (Par ps s0) = do
  let (ss,ms) = unzip ps
  (mis :: [String]) <- ifLefts "eShow" $ map (eShow r) ms
  let showPair :: (String, String) -> String
      showPair (s,mi) = s ++ " " ++ [bracket_angle_big_left]
        ++ mi ++ [bracket_angle_big_right] ++ " "
  Right $ concat (map showPair $ zip ss mis) ++ " " ++ s0

