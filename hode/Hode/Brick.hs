-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables
, MultiParamTypeClasses
, ViewPatterns
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hode.Brick (
    ColorString, Color(..)
  , toColorString -- ^ String -> ColorString
  , colorToAttrName  -- ^ Color -> B.AttrName
  , ShowColor, showColor
  , unColorString -- ^ ColorString -> String

  -- | = mapping across a ColorString
  , colorParen -- ^ ColorString -> ColorString
  , colorStrip -- ^ [(String,a)] -> [(String,a)]
  , colorLeftRight -- ^ Maybe (s -> s) ->
                  --         (s -> s) ->
                  --         (s -> s) ->
                  --         [(s,a)] -> [(s,a)]
  , colorConsolidate -- ^ forall a b. (Eq a, Eq b, Monoid a)
                    -- => [(a,b)] -> [(a,b)]
  , colorStringLength -- ^ ColorString -> Int
  ) where

import           Data.Text (strip, stripStart, stripEnd, pack, unpack)
import           Lens.Micro hiding (both)

import qualified Brick.AttrMap as B


-- | Like `String`, but different substrings can have different fonts.
type ColorString = [(String, Color)]

toColorString :: String -> ColorString
toColorString s = [(s, NoColor)]

colorToAttrName :: Bool -> Color -> B.AttrName
colorToAttrName False TextColor = B.attrName "white on green"
colorToAttrName False SepColor  = B.attrName "white on red"
colorToAttrName False AddrColor = B.attrName "white on blue"
colorToAttrName False NoColor   = B.attrName "white on black"
colorToAttrName True  TextColor = B.attrName "white on blue"
colorToAttrName True  SepColor  = B.attrName "white on green"
colorToAttrName True  AddrColor = B.attrName "white on red"
colorToAttrName True  NoColor   = B.attrName "black on white"

data Color = TextColor | SepColor | AddrColor | NoColor
  deriving (Show,Eq,Ord,Enum)

class ShowColor options a where
  showColor :: options -> a -> ColorString

unColorString :: ColorString -> String
unColorString = concatMap fst


-- | = mapping across an ColorString

colorParen :: ColorString -> ColorString
colorParen x = [("(",SepColor)] ++ x ++ [(")",SepColor)]

-- | `colorStrip` is like `strip` from Data.Text
colorStrip :: [(String,a)] -> [(String,a)]
  -- ^ a little more general than `ColorString -> ColorString`
colorStrip = colorLeftRight both left right where
  both  = Just $ unpack . strip      . pack
  left  =        unpack . stripStart . pack
  right =        unpack . stripEnd   . pack

-- | `colorLeftRight both left right sas` applies `left` to the first
-- member of `sas` and `right` to the last member, with one exception:
-- if `both == Just f` and `sas` is a singleton,
-- it applies `both` and leaves `left` and `right` unuesd.
colorLeftRight ::
  Maybe (s -> s)
  ->    (s -> s)
  ->    (s -> s)
  -> [(s,a)] -> [(s,a)]
colorLeftRight _ _ _ [] = []
colorLeftRight both left right [(s,a)] =
  [ ( s & maybe (left . right) id both
    , a) ]
colorLeftRight _    left right ((s,a):more) =
  (left $ s, a) : f more where
  f (p1:p2:ps) = p1 : f (p2:ps)
  f [(s',a')] = [(right s', a')]
  f [] = error "impossible -- f bottoms out at length one, not zero"

colorConsolidate :: forall a b. (Eq a, Eq b, Monoid a)
                => [(a,b)] -> [(a,b)]
colorConsolidate =
  colorUngroup . colorGroup []
  where
  colorGroup :: [[(a,b)]] -- accumulator
            -> [(a,b)]   -- input to consume
            -> [[(a,b)]]
  colorGroup acc [] = acc -- done
  colorGroup acc (a:as) =
    if fst a == mempty then colorGroup acc as -- skip mempty
    else if null acc then colorGroup [[a]] as -- first group
    else let h = head acc
             hh = head h
         in if snd a == snd hh -- same color
            then colorGroup ((a:h):tail acc) as -- same group
            else colorGroup ([a]:acc) as -- new group

  -- | PITFALL: Assumes every `[(a,b)]` has the same `b`, and none is empty.
  -- Those will be true if the `[[(a,b)]]` was created via `colorGroup`.
  colorUngroup :: [[(a,b)]] -> [(a,b)]
  colorUngroup = reverse . map f where
    f :: [(a,b)] -> (a,b)
    f [] = error "colorUngroup: should not happen"
    f cs@((_,b):_) = ( mconcat $ map fst $ reverse cs,
                       b )

colorStringLength :: ColorString -> Int
colorStringLength = sum . map (length . fst)
