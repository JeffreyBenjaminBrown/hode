{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Show where

import           Data.Functor.Foldable
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
eShow r = para f where
  f :: Base Expr (Expr, Either String String) -> Either String String

  f e@(AddrF _) = prefixLeft "eShow Addr" $
    unAddr r (embed $ fmap fst e) >>= eShow r

  f (PhraseF w) = Right w

  f (ExprTpltF pairs) = ifLefts "eShow ExprTplt" (map snd pairs)
                  >>= Right . concat . L.intersperse " _ "

  f relf@(ExprRelF (Rel ms (ExprTplt js, _))) = do
  -- The recursive argument (second member of the pair) is unused, hence
  -- not computed. Instead, each joint in `js` is `eShow`n separately.
    mss <- ifLefts "eShow ExprRel" $ map snd ms
    jss <- let rel = embed $ fmap fst relf
           in hashUnlessEmptyStartOrEnd (depth rel)
              <$> ifLefts "eShow ExprRel" ( map (eShow r) js )
    Right $ unpack . strip . pack $ concat
      $ map (\(m,j) -> m ++ " " ++ j ++ " ")
      $ zip ("" : mss) jss

  f (ExprRelF (Rel ms (a@(Addr _), _))) = do
    tpltExpr <- unAddr r a
    eShow r $ ExprRel $ Rel (map fst ms) tpltExpr

  f x@(ExprRelF _) = Left $ "eShow: ExprRel with non-Tplt for Tplt: "
                     ++ show (embed $ fmap fst x)

  f (ParF triples s0) = do
    let (ss :: [String], ps)               = unzip triples
        (_, ess :: [Either String String]) = unzip ps
    (mis :: [String]) <- ifLefts "eShow Par" ess
    let showPair :: (String, String) -> String
        showPair (s,mi) = s ++ " " ++ [bracket_angle_big_left]
          ++ mi ++ [bracket_angle_big_right] ++ " "
    Right $ concat (map showPair $ zip ss mis) ++ " " ++ s0
