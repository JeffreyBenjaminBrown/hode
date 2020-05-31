{-# LANGUAGE ViewPatterns
, ScopedTypeVariables
#-}

module Hode.Brick.Wrap (
    colorStringWrap       -- ^ Int -> (Bool, ColorString) -> Widget n
  , toLines                -- ^ Int -> ColorString -> [ColorString]
  , extractLine            -- ^ Int -> [(String,a)]
                           -- -> ([(String,a)], [(String,a)])
  , splitAtLastSpaceBefore -- ^ Int -> (String,c)
                           -- -> ((String,c), (String,c))
  ) where

import           Control.Arrow (first)

import           Brick.Types
import           Brick.Widgets.Core

import Hode.Brick


colorStringWrap :: forall n.
  Int -> (Bool, ColorString) -> Widget n
colorStringWrap maxLength (isFocused, cs) =
  let drawLineSegment :: (String,Color) -> Widget n
      drawLineSegment (s,c) =
        withAttr (colorToAttrName isFocused c) $ str s
      drawLine :: ColorString -> Widget n =
        hBox . map drawLineSegment
  in vBox $ map drawLine $ toLines maxLength cs

-- | `toLines maxLength as0` divides a `ColorString` into lines
-- of maximum length roughly equal to `maxLength`.
toLines :: Int -> ColorString -> [ColorString]
toLines maxLength as0 = let
  (one :: ColorString, rest :: ColorString) =
    extractLine maxLength as0
  in if null rest then [one]
     else one : toLines maxLength rest

extractLine :: forall a. Eq a =>
  Int -> [(String,a)] -> ([(String,a)], [(String,a)])
extractLine maxLength as0 = go 0 as0 where
  go _ [] = ([],[])
  go ((>= maxLength) -> True) as = ([],as)
  go k (a:as) =
    let (one,more) =
          splitAtLastSpaceBefore maxLength a
        h = length $ fst one
        as' = if null $ fst more
              then as else more : as
    in first (one:) $ go (k+h) as'

splitAtLastSpaceBefore ::
  Int -> (String,c) -> ((String,c), (String,c))
splitAtLastSpaceBefore maxLength (s,c) =
  let (atMost :: String, rest :: String) =
        splitAt maxLength s
      (no :: String, yes :: String) =
        span ((/=) ' ') $ reverse atMost
  in if null yes
     then ( (atMost,             c),
            (              rest, c) )
     else ( (reverse yes,        c),
            (reverse no ++ rest, c) )
