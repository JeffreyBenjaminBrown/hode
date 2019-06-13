-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Bughunt.Brick (
    AttrString
  , attr1, attr2 -- ^ V.Attr
  , attrStringWrap -- ^ AttrString -> Widget n
  ) where

import           Lens.Micro hiding (both)

import qualified Graphics.Vty as V
import           Brick.Util (on)
import qualified Brick.BorderMap as B
import           Brick.Types


-- | Like `String`, but different substrings can have different fonts.
type AttrString = [(String, V.Attr)]

-- | '#' symbols and parens used to group `Expr`s are "separators".
-- (Note that ordinary text can include those symbols, too;
-- in that case they will not be colored differently.)
attr1, attr2 :: V.Attr
attr1  = V.brightRed `on` V.blue
attr2 = V.brightBlue `on` V.red

-- | `attrStringWrap` is based on `myFill`, from
-- [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context)
attrStringWrap ::  AttrString -> Widget n
attrStringWrap ss =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLines w ss
    return $ Result i [] [] [] B.empty

  where

  linesToImage :: [AttrString] -> V.Image
  linesToImage = let g (s,a) = V.string a s
    in V.vertCat . map (V.horizCat . map g)

  -- | PITFALL: Does not consider the case in which a single token
  -- does not fit on one line. Its right side will be truncated.
  toLines :: Int -> AttrString -> [AttrString]
  toLines maxWidth = reverse . map reverse . f 0 [] where
    f _       acc               []                  = acc
    f _       []                ((s,a):moreInput)   =
      f (length s) [[(s,a)]] moreInput
    f lineLen o@(line:moreOutput) ((s,a):moreInput) =
      let newLen = lineLen + length s
      in if newLen > maxWidth
         then f (length s) ([(s,a)]     :o)          moreInput
         else f newLen     (((s,a):line):moreOutput) moreInput
