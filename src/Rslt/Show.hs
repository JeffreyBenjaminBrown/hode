{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Show where

import qualified Data.List as L
import           Data.Text (strip, pack, unpack)

import Rslt.RLookup
import Rslt.RTypes
import Rslt.RUtil
import Util.Misc
import Util.UParse


-- https://unicode-search.net/unicode-namesearch.pl?term=bracket
bracket_angle_big_left, bracket_angle_big_right, bracket_angle_small_left, bracket_angle_small_right :: Char
bracket_angle_big_left    = '⦑' -- C-x 8 Ret 2991
bracket_angle_big_right   = '⦒' -- C-x 8 Ret 2992
bracket_angle_small_left  = '«' -- C-x 8 <
bracket_angle_small_right = '»' -- C-x 8 >


hashUnlessEmptyStartOrEnd :: Int -> [String] -> [String]
hashUnlessEmptyStartOrEnd k0 joints = case joints' of
  [] -> []
  s : ss ->   hashUnlessEmpty    k0 s
            : hashUnlessEmptyEnd k0 ss

  where
  joints' = map maybeParens joints where
    maybeParens :: String -> String
    maybeParens s = if hasMultipleWords s
      then "(" ++ s ++ ")" else s

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


eShow :: Rslt -> Expr -> Either String String
eShow r (Addr a) = do
  e <- addrToRefExpr r a
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
  (te :: RefExpr) <- prefixLeft "eShow" $ addrToRefExpr r a
  (ti :: Expr)    <- prefixLeft "eShow" $ refExprToExpr r te
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

