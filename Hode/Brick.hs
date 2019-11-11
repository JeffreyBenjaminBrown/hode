-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables
, ViewPatterns
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hode.Brick (
    ColorString, Color(..)
  , ShowAttr, showAttr
  , unColorString -- ^ ColorString -> String

  , attrStringWrap' -- ^ Int -> ColorString -> Widget n
  , toLines' -- ^ Int -> ColorString -> [ColorString]
  , extractLine            -- ^ Int -> [(String,a)]
                 -- -> ([(String,a)], [(String,a)])
  , splitAtLastSpaceBefore -- ^ Int -> (String,c)
                   -- -> ((String,c), (String,c))

  -- | = mapping across an ColorString
  , attrParen -- ^ ColorString -> ColorString
  , attrStrip -- ^ [(String,a)] -> [(String,a)]
  , attrLeftRight -- ^ Maybe (s -> s) ->
                  --         (s -> s) ->
                  --         (s -> s) ->
                  --         [(s,a)] -> [(s,a)]
  , attrConsolidate -- ^ forall a b. (Eq a, Eq b, Monoid a)
                    -- => [(a,b)] -> [(a,b)]
  , attrStringLength -- ^ ColorString -> Int
  , sepColor, textColor, addrColor -- ^ V.Attr
  , colorToAttrName -- ^ Color -> B.AttrName
  ) where

import           Control.Arrow (first)
import           Data.Text (strip, stripStart, stripEnd, pack, unpack)
import           Lens.Micro hiding (both)

import qualified Graphics.Vty as V
import           Brick.Types
import           Brick.Widgets.Core (hBox,vBox,str,withAttr)
import           Brick.Util (on)
import qualified Brick.AttrMap as B


-- | Like `String`, but different substrings can have different fonts.
type ColorString = [(String, Color)]

data Color = TextColor | SepColor | AddrColor
  deriving (Show,Eq,Ord,Enum)

instance Ord V.Attr where
  a <= b = show a <= show b

class ShowAttr a where
  showAttr :: a -> ColorString

unColorString :: ColorString -> String
unColorString = concatMap fst


-- | TODO ? `attrStringWrap' maxLength`
-- does not quite behave as expected:
-- sometimes the line is slightly longer than `maxLength`.
attrStringWrap' :: forall n. Int -> ColorString -> Widget n
attrStringWrap' maxLength =
  let drawLineSegment :: (String,Color) -> Widget n
      drawLineSegment (s,c) =
        withAttr (colorToAttrName c) $ str s
      drawLine :: ColorString -> Widget n
      drawLine = hBox . map drawLineSegment
  in vBox . map drawLine . toLines' maxLength

toLines' :: Int -> ColorString -> [ColorString]
toLines' maxLength as0 = let
  (one :: ColorString, rest :: ColorString) =
    extractLine maxLength as0
  in if null rest then [one]
     else one : toLines' maxLength rest

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
            (rest,               c) )
     else ( (reverse yes,        c),
            (reverse no ++ rest, c) )

-- | = mapping across an ColorString

attrParen :: ColorString -> ColorString
attrParen x = [("(",SepColor)] ++ x ++ [(")",SepColor)]

-- | `attrStrip` is like `strip` from Data.Text
attrStrip :: [(String,a)] -> [(String,a)]
  -- ^ a little more general than `ColorString -> ColorString`
attrStrip = attrLeftRight both left right where
  both  = Just $ unpack . strip      . pack
  left  =        unpack . stripStart . pack
  right =        unpack . stripEnd   . pack

-- | `attrLeftRight both left right sas` applies `left` to the first
-- member of `sas` and `right` to the last member, with one exception:
-- if `both == Just f` and `sas` is a singleton,
-- it applies `both` and leaves `left` and `right` unuesd.
attrLeftRight ::
  Maybe (s -> s)
  ->    (s -> s)
  ->    (s -> s)
  -> [(s,a)] -> [(s,a)]
attrLeftRight _ _ _ [] = []
attrLeftRight both left right [(s,a)] =
  [ ( s & maybe (left . right) id both
    , a) ]
attrLeftRight _    left right ((s,a):more) =
  (left $ s, a) : f more where
  f (p1:p2:ps) = p1 : f (p2:ps)
  f [(s',a')] = [(right s', a')]
  f [] = error "impossible -- f bottoms out at length one, not zero"

attrConsolidate :: forall a b. (Eq a, Eq b, Monoid a)
                => [(a,b)] -> [(a,b)]
attrConsolidate =
  attrUngroup . attrGroup []
  where
  attrGroup :: [[(a,b)]] -- accumulator
            -> [(a,b)]   -- input to consume
            -> [[(a,b)]]
  attrGroup acc [] = acc -- done
  attrGroup acc (a:as) =
    if fst a == mempty then attrGroup acc as -- skip mempty
    else if null acc then attrGroup [[a]] as -- first group
    else let h = head acc
             hh = head h
         in if snd a == snd hh -- same color
            then attrGroup ((a:h):tail acc) as -- same group
            else attrGroup ([a]:acc) as -- new group

  -- | PITFALL: Assumes every `[(a,b)]` has the same `b`, and none is empty.
  -- Those will be true if the `[[(a,b)]]` was created via `attrGroup`.
  attrUngroup :: [[(a,b)]] -> [(a,b)]
  attrUngroup = reverse . map f where
    f :: [(a,b)] -> (a,b)
    f [] = error "attrUngroup: should not happen"
    f cs@((_,b):_) = ( mconcat $ map fst $ reverse cs,
                       b )

attrStringLength :: ColorString -> Int
attrStringLength = sum . map (length . fst)

-- | '#' symbols and parens used to group `Expr`s are "separators".
-- (Note that ordinary text can include those symbols, too;
-- in that case they will not be colored differently.)
sepColor, textColor, addrColor :: V.Attr
(sepColor, textColor, addrColor) =
  let rc :: Int -> Int -> Int -> V.Color
      rc = V.rgbColor
  in ( rc 255 255 255 `on` rc 1 0 0
     , rc 255 255 255 `on` rc 0 1 0
     , rc 255 255 255 `on` rc 0 0 1 )

colorToAttrName :: Color -> B.AttrName
colorToAttrName TextColor = B.attrName "textColor"
colorToAttrName SepColor  = B.attrName "sepColor"
colorToAttrName AddrColor = B.attrName "addrColor"
