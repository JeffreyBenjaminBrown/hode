-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Brick.ScreenWrap (
    toWidget -- ^        [(String,V.Attr)] -> Widget n
  , toLines  -- ^ Int -> [(String,attr)] -> [[(String,attr)]]
  ) where

import           Lens.Micro

import Brick.Types
import qualified Graphics.Vty as V
import qualified Brick.BorderMap as B


-- | Based on `myFill` from [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context).
toWidget ::  [(String,V.Attr)] -> Widget n
toWidget ss =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLines w ss
    return $ Result i [] [] [] B.empty

linesToImage :: [[(String,V.Attr)]] -> V.Image
linesToImage = let g (s,a) = V.string a s
  in V.vertCat . map (V.horizCat . map g)

-- | PITFALL: Does not consider the case in which a single token
-- does not fit on one line. Its right side will be truncated.
toLines :: Int -> [(String,attr)] -> [[(String,attr)]]
toLines maxWidth = reverse . map reverse . f 0 [] where
  f _       acc               []                = acc
  f _       []                ((s,a):moreInput) =
    f (length s) [[(s,a)]] moreInput
  f lineLen o@(line:moreOutput) ((s,a):moreInput) =
    let newLen = lineLen + length s
    in if newLen > maxWidth
       then f (length s) ([(s,a)]     :o)          moreInput
       else f newLen     (((s,a):line):moreOutput) moreInput
