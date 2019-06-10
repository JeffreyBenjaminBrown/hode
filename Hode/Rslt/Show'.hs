{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show'
--  (
--    eShow      -- ^        Rslt -> Expr -> Either String String
--  , eParenShow -- ^ Int -> Rslt -> Expr -> Either String String
--
--  , hashUnlessEmptyStartOrEnd -- ^ Int -> [String] -> [String]
--  , Parens(..)
--  , parenExprAtDepth -- ^ Int -> Fix (ExprFWith ())
--                     --       -> Fix (ExprFWith (Int,Parens))
--  )
where

import           Data.Functor.Foldable
import qualified Data.List as L
import           Data.Text (strip, pack, unpack)

import           Brick.Util (on)
import           Graphics.Vty (Attr)
import qualified Graphics.Vty as V

import Hode.Brick.ScreenWrap
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc
import Hode.Util.UParse


-- | '#' symbols and parens used to group `Expr`s are "separators".
-- (Note that ordinary text can include those symbols, too;
-- in that case they will not be colored differently.)
sepColor, textColor :: V.Attr
sepColor = V.brightRed `on` V.black
textColor = V.brightBlue `on` V.black

-- | `hashUnlessEmptyStartOrEnd k js` adds `k` #-marks to every joint
-- in `js`, unless it's first or last and the empty string.
hashUnlessEmptyStartOrEnd' :: Int -> [String] -> [AttrString]
hashUnlessEmptyStartOrEnd' k0 joints = case joints' of
  [] -> []
  s : ss ->   hashUnlessEmpty    k0 s
            : hashUnlessEmptyEnd k0 ss

  where
  joints' = map maybeParens joints where
    maybeParens :: String -> String
    maybeParens s = if hasMultipleWords s
      then "(" ++ s ++ ")" else s

  hash :: Int -> String -> AttrString
  hash k s = [ (replicate k '#', sepColor)
             , (s,              textColor) ]

  hashUnlessEmpty :: Int -> String -> AttrString
  hashUnlessEmpty _ "" = [("",textColor)]
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [String] -> [AttrString]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss
