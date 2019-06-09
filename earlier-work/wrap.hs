-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Main where

import           Lens.Micro

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.AttrMap
import Brick.Focus
import Brick.Util (on)
import qualified Graphics.Vty as V
import qualified Brick.BorderMap as B
import Control.DeepSeq (force)


myFill :: Char -> Widget n
myFill ch =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let a = ctx^.attrL
        i :: V.Image = V.charFill a ch
          (ctx^.availWidthL) (ctx^.availHeightL)
    return $ Result i [] [] [] B.empty

toWidget ::  [(String,V.Attr)] -> Widget n
toWidget ss = let
  x = do
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLines w ss
    return $ Result i [] [] [] B.empty
  in Widget Greedy Fixed x

-- `toWidget'` was totally unnecessary;
-- I wrote it because I thought `toWidget` wasn't working,
-- when in fact the problem was `toLines`.
toWidget' ::  [(String,V.Attr)] -> Widget n
toWidget' ss =
  Widget Greedy Fixed $ do
    c <- getContext
    let theLines = fmap (fmap $ _1 %~ fixEmpty) $
                   toLines (c^.availWidthL) ss
          where fixEmpty l | null l = " "
                           | otherwise = l
        unit (s,a) = V.string a s
        lineLength sas = sum $ map (length . fst) sas

    case force theLines of
      [] -> return emptyResult
      [one] -> return $ emptyResult &
               imageL .~ V.horizCat (map unit one)
      multiple -> let
        maxLen = maximum $ lineLength <$> multiple
        lineImgs = lineImg <$> multiple
        lineImg sas = let
          fillEnd = ( replicate (maxLen - lineLength sas) ' '
                    , V.defAttr )
          in V.horizCat $ map unit $ sas ++ [fillEnd]
        in return $ emptyResult & imageL .~ V.vertCat lineImgs

linesToImage :: [[(String,V.Attr)]] -> V.Image
linesToImage = let g (s,a) = V.string a s
  in V.vertCat . map (V.horizCat . map g)

-- | PITFALL: Does not consider the case in which a single token
-- does not fit on the screen. It will get truncated.
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

-- rather than `Graphics.Vty.ImagecharFill`, use
-- `Graphics.Vty.Image.string` and the `horizCat` and `vertCat`
-- combinators from the same module.

red, blue :: V.Attr
red = V.defAttr `V.withForeColor` V.red
blue = V.defAttr `V.withForeColor` V.blue

main :: IO ()
main = simpleMain $ -- (myFill 'z' :: Widget ())
  vBox [h, h] where
  h = hBox [ toWidget pairs ]
      :: Widget ()

pairs :: [(String, V.Attr)]
pairs = [ ("%%       #######a ",red)
        , ("%%       #########b ",blue)
        , ("%%       ############cc ",blue)
        , ("%%       ###############dd ", red) ]
