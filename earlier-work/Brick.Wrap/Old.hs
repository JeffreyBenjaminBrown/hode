-- | `attrStringWrap` builds a `Widget` intended to be like Brick's
-- `strWrap`, but for `AttrString`s.
--
-- There's an unmerged branch in Hode where I worked on the problem,
-- called showPTree_bughunt.
--
-- There are some issues on Github/Brick
-- about it:
-- https://github.com/jtdaugherty/brick/issues/234
-- https://github.com/jtdaugherty/brick/issues/232


{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Brick.Wrap.Old (
    attrStringWrapOld -- ^ [(String,V.Attr)] -> Widget n
  , linesToImage      -- ^ [AttrString] -> V.Image
  , toLinesOld        -- ^ Int -> AttrString -> [AttrString]
  , colorToVtyAttr    -- ^ Color -> V.Attr
  ) where


import           Control.Arrow (second)
import           Lens.Micro hiding (both)

import qualified Graphics.Vty as V
import           Brick.Types
import qualified Brick.BorderMap as B

import Hode.Brick


-- | = `attrStringWrap` is the purpose of `AttrString`

-- | Based on `myFill` from [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context).
attrStringWrapOld ::  AttrString -> Widget n
attrStringWrapOld ss =
  Widget Fixed Fixed $ do
  -- TODO ? PITFALL: I don't know why a `Fixed, Fixed` size policy works.
  -- I expected to need it to be greedy in the horizontal dimension,
  -- but so far this gives better results.
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLinesOld w ss
    return $ Result i [] [] [] B.empty

linesToImage :: [AttrString] -> V.Image
linesToImage = let
  g (s :: String, a :: V.Attr) = V.string a s
  in V.vertCat
     . map (V.horizCat . map g)
     . map (map $ second colorToVtyAttr)

-- | PITFALL: Does not consider the case in which a single token
-- does not fit on one line. Its right side will be truncated.
toLinesOld :: Int -> AttrString -> [AttrString]
toLinesOld maxWidth = reverse . map reverse . f 0 [] where
  f _       acc               []                = acc
  f _       []                ((s,a):moreInput) =
    f (length s) [[(s,a)]] moreInput
  f lineLen o@(line:moreOutput) ((s,a):moreInput) =
    let newLen = lineLen + length s
    in if newLen > maxWidth
       then f (length s) ([(s,a)]     :o)          moreInput
       else f newLen     (((s,a):line):moreOutput) moreInput

colorToVtyAttr :: Color -> V.Attr
colorToVtyAttr TextColor = textColor
colorToVtyAttr SepColor  = sepColor
colorToVtyAttr AddrColor = addrColor
