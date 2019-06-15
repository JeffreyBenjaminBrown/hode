-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hode.Brick (
    AttrString
  , sepColor, textColor, addrColor -- ^ V.Attr
  , unAttrString -- ^ AttrString -> String

  -- | = `attrStringWrap` is the purpose of `AttrString`
  , attrStringWrap -- ^        [(String,V.Attr)] -> Widget n
  , toLines        -- ^ Int -> [(String,attr)] -> [[(String,attr)]]

  -- | = mapping across an AttrString
  , attrStrip -- ^ [(String,a)] -> [(String,a)]
  , attrParen -- ^ [(String,a)] -> [(String,a)]
  , attrLeftRight -- ^ Maybe (s -> s) ->
                  --         (s -> s) ->
                  --         (s -> s) ->
                  --         [(s,a)] -> [(s,a)]
  ) where

import           Data.Text (strip, stripStart, stripEnd, pack, unpack)
import           Lens.Micro hiding (both)

import qualified Graphics.Vty as V
import           Brick.Util (on)
import qualified Brick.BorderMap as B

import Brick.Types


-- | Like `String`, but different substrings can have different fonts.
type AttrString = [(String, V.Attr)]

instance Ord V.Attr where
  a <= b = show a <= show b

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

unAttrString :: AttrString -> String
unAttrString = concatMap fst


-- | = `attrStringWrap` is the purpose of `AttrString`

-- | Based on `myFill` from [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context).
attrStringWrap ::  AttrString -> Widget n
attrStringWrap ss =
  Widget Fixed Fixed $ do
  -- TODO ? PITFALL: I don't know why a `Fixed, Fixed` size policy works.
  -- I expected to need it to be greedy in the horizontal dimension,
  -- but so far this gives better results.
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLines w ss
    return $ Result i [] [] [] B.empty

linesToImage :: [AttrString] -> V.Image
linesToImage = let g (s,a) = V.string a s
  in V.vertCat . map (V.horizCat . map g)

-- | PITFALL: Does not consider the case in which a single token
-- does not fit on one line. Its right side will be truncated.
toLines :: Int -> AttrString -> [AttrString]
toLines maxWidth = reverse . map reverse . f 0 [] where
  f _       acc               []                = acc
  f _       []                ((s,a):moreInput) =
    f (length s) [[(s,a)]] moreInput
  f lineLen o@(line:moreOutput) ((s,a):moreInput) =
    let newLen = lineLen + length s
    in if newLen > maxWidth
       then f (length s) ([(s,a)]     :o)          moreInput
       else f newLen     (((s,a):line):moreOutput) moreInput


-- | = mapping across an AttrString

attrStrip :: [(String,a)] -> [(String,a)]
attrStrip = attrLeftRight both left right where
  both  = Just $ unpack . strip      . pack
  left  =        unpack . stripStart . pack
  right =        unpack . stripEnd   . pack

attrParen :: [(String,a)] -> [(String,a)]
attrParen = attrLeftRight Nothing ("(" ++) (++ ")")

attrLeftRight ::
  Maybe (s -> s) ->
        (s -> s) ->
        (s -> s) ->
  [(s,a)] -> [(s,a)]
attrLeftRight _ _ _ [] = []
attrLeftRight both left right [(s,a)] =
  [ ( s & maybe (left . right) id both
    , a) ]
attrLeftRight _    left right ((s,a):more) =
  (left $ s, a) : f more where
  f (p1:p2:ps) = p1 : f (p2:ps)
  f [(s',a')] = [(right s', a')]
  f [] = error "impossible -- f bottoms out at length one, not zero"
