-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables
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
  , attrParen -- ^ ColorString -> ColorString
  , attrStrip -- ^ [(String,a)] -> [(String,a)]
  , attrLeftRight -- ^ Maybe (s -> s) ->
                  --         (s -> s) ->
                  --         (s -> s) ->
                  --         [(s,a)] -> [(s,a)]
  , attrConsolidate -- ^ forall a b. (Eq a, Eq b, Monoid a)
                    -- => [(a,b)] -> [(a,b)]
  , attrStringLength -- ^ ColorString -> Int
  ) where

import           Data.Text (strip, stripStart, stripEnd, pack, unpack)
import           Lens.Micro hiding (both)

import qualified Brick.AttrMap as B


-- | Like `String`, but different substrings can have different fonts.
type ColorString = [(String, Color)]

toColorString :: String -> ColorString
toColorString s = [(s, TextColor)]

colorToAttrName :: Bool -> Color -> B.AttrName
colorToAttrName False TextColor = B.attrName "textColor"
colorToAttrName False SepColor  = B.attrName "sepColor"
colorToAttrName False AddrColor = B.attrName "addrColor"
colorToAttrName True  TextColor = B.attrName "addrColor"
colorToAttrName True  SepColor  = B.attrName "textColor"
colorToAttrName True  AddrColor = B.attrName "sepColor"

data Color = TextColor | SepColor | AddrColor
  deriving (Show,Eq,Ord,Enum)

class ShowColor a where
  showColor :: a -> ColorString

unColorString :: ColorString -> String
unColorString = concatMap fst


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
