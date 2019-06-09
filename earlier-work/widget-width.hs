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
import Graphics.Vty
import Brick.BorderMap as B


myFill :: Char -> Widget n
myFill ch =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let a = ctx^.attrL
        i :: Image = charFill a ch
          (ctx^.availWidthL) (ctx^.availHeightL)
    return $ Result i [] [] [] B.empty

-- rather than `Graphics.Vty.ImagecharFill`, use
-- `Graphics.Vty.Image.string` and the `horizCat` and `vertCat`
-- combinators from the same module.

main :: IO ()
main = simpleMain $ (myFill 'z' :: Widget ())
