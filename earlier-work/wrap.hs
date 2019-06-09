-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Main where

import           Lens.Micro

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import qualified Brick.BorderMap as B
import Control.DeepSeq (force)


main :: IO ()
main = simpleMain $ vBox [h, h]
  where h :: Widget ()
        h = hBox [ toWidget content ]

-- | Based on `myFill` from [the rendering docs](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#using-the-rendering-context).
toWidget ::  [(String,V.Attr)] -> Widget n
toWidget ss = let
  x = do
    ctx <- getContext
    let w = ctx^.availWidthL
        i :: V.Image = linesToImage $ toLines w ss
    return $ Result i [] [] [] B.empty
  in Widget Greedy Fixed x

-- | `toWidget'` appears to have been totally unnecessary.
-- I wrote it because I thought `toWidget` wasn't working,
-- when in fact the problem was `toLines`.
-- It is based on `Brick.Widgets.Core.txtWrapWith`.
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

content :: [(String, V.Attr)]
content = [ ("%%       #######a ",red)
          , ("%%       #########b ",blue)
          , ("%%       ############cc ",blue)
          , ("%%       ###############dd ", red) ]
  where red, blue :: V.Attr
        red = V.defAttr `V.withForeColor` V.red
        blue = V.defAttr `V.withForeColor` V.blue
