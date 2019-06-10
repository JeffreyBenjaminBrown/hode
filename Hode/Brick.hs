-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Brick (
    AttrString
  , toWidget -- ^        [(String,V.Attr)] -> Widget n
  , toLines  -- ^ Int -> [(String,attr)] -> [[(String,attr)]]
  , attrStrip -- ^ [(String,a)] -> [(String,a)]
  , attrParen -- ^ [(String,a)] -> [(String,a)]
  , attrLeftRight -- ^ Maybe (s -> s) ->
                  --         (s -> s) ->
                  --         (s -> s) ->
                  --         [(s,a)] -> [(s,a)]
  ) where

import           Data.Text (strip, stripStart, stripEnd, pack, unpack)
import           Lens.Micro

import qualified Graphics.Vty as V
import qualified Brick.BorderMap as B

import Brick.Types


-- | Like `String`, but different substrings can have different fonts.
type AttrString = [(String, V.Attr)]


-- | Based on `myFill` from [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context).
toWidget ::  AttrString -> Widget n
toWidget ss =
  Widget Greedy Fixed $ do
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
attrLeftRight both left right [] = []
attrLeftRight both left right [(s,a)] =
  [ ( s & maybe (left . right) id both
    , a) ]
attrLeftRight _ left right ((s,a):more) =
  (left $ s, a) : f more where
  f (p1:p2:ps) = p1 : f (p2:ps)
  f [(s',a')] = [(right s', a')]
  f [] = error "impossible -- f bottoms out at length one, not zero"
