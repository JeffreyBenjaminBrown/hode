-- TODO ? Maybe this module could be made simpler,
-- now that Tplt is more complex than a synonym for List.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show.Util (
    hashUnlessEmptyStartOrEnd -- ^ Int -> [String] -> [String]
  , hash                      -- ^ Int ->  String -> String
  , trimString                -- ^         String -> String
  ) where

import Data.Text (strip, pack, unpack)

import Hode.Util.UParse

-- | `hashUnlessEmptyStartOrEnd k js` prefixes
-- `k` '#' characters to every joint in `js`,
-- unless it's empty and first or empty and last.
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

  hashUnlessEmpty :: Int -> String -> String
  hashUnlessEmpty _ "" = ""
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [String] -> [String]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss

hash :: Int -> String -> String
hash k s = replicate k '#' ++ s

trimString :: String -> String
trimString = unpack . strip . pack
