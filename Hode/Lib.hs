-- | Wraps a list of `String`s with `Attr`s attached.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Lib where

import           Lens.Micro

import qualified Brick.BorderMap as B
import           Brick.Types
import           Brick.Widgets.Core
import qualified Graphics.Vty as V


-- | Like `String`, but different substrings can have different fonts.
type AttrString = [(String, V.Attr)]

-- | `attrStringWrap` wraps an `AttrString` around the screen --
-- or, more precisely, around the space allocated to the resulting Widget.
-- `attrStringWrap` is based on `myFill`, from
-- [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context)
attrStringWrap ::  AttrString -> Widget n
attrStringWrap ss =
  Widget Fixed Greedy $ do
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

-- | The following two functions are identical, except for
-- the definition of the internal sub-function called `showRow`.
showTwoAspects :: forall a b n.
     (b -> Widget n)
  -> (a -> b) -- ^ shows one aspect of an `a`
  -> (a -> b) -- ^ shows another
  -> [a]      -- ^ what to show
  -> Widget n
showTwoAspects b2w showLeft showRight =
  vBox . map showRow
  where
    showRow :: a -> Widget n
    showRow a = hBox [ b2w $ showLeft a
                     , b2w $ showRight a ]

showOneAspect :: forall a b n.
     (b -> Widget n)
  -> (a -> b) -- ^ ignored
  -> (a -> b) -- ^ shows an aspect of a
  -> [a]      -- ^ what to show
  -> Widget n
showOneAspect b2w _showLeft showRight =
  vBox . map showRow
  where
    showRow :: a -> Widget n
    showRow a = b2w $ showRight a
