-- | Unused, but potentially helpful (particularly `demo`)
-- for understanding Hode.Brick.ScreenWrap.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Brick.Wrap.Demo
    demo            -- ^ IO ()
  , attrStringWrap' -- ^ [(String,V.Attr)] -> Widget n
  ) where

import           Lens.Micro

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import Control.DeepSeq (force)

import Hode.Brick


demo :: IO ()
demo = simpleMain $ vBox [h, h] where

  h :: Widget ()
  h = hBox [ attrStringWrap content ] where

    content :: [(String, V.Attr)]
    content = [ ("%%       #######a ",red)
              , ("%%       #########b ",blue)
              , ("%%       ############cc ",blue)
              , ("%%       ###############dd ", red) ] where
      red, blue :: V.Attr
      red = V.defAttr `V.withForeColor` V.red
      blue = V.defAttr `V.withForeColor` V.blue


-- | `attrStringWrap'` appears to have been totally unnecessary.
-- I wrote it because I thought `attrStringWrap` wasn't working,
-- when in fact the problem was `toLines`.
-- It is based on `Brick.Widgets.Core.txtWrapWith`.
attrStringWrap' ::  [(String,V.Attr)] -> Widget n
attrStringWrap' ss =
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
