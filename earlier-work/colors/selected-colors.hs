-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.Main where

import qualified Data.Map as M

import qualified Brick.Main           as B
import qualified Brick.Types          as B
import           Brick.Widgets.Core
import qualified Brick.AttrMap        as B
import           Brick.Util (on)
import qualified Graphics.Vty         as B


main :: IO ()
main = B.defaultMain app ()

app :: B.App () e ()
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

appHandleEvent :: () -> B.BrickEvent () e
               -> B.EventM () (B.Next ())
appHandleEvent _ _ = B.halt ()

-- | The focused subview is recalculated at each call to `appDisplay`.
-- Each `ViewExprNodeTree`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: () -> [B.Widget ()]
appDraw _ = [v] where
  v = vBox [ h j | j <- [1..numColors] ]
    where
    h j = hBox [ withAttr (B.attrName $ show i ++ show j) $
                 str $ show i ++ " #obj " ++ show j
               | i <- [1..numColors], i /= 1 ]

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr nameAttrs
  where
  nameAttrs :: [ (B.AttrName, B.Attr) ]
  nameAttrs = [ ( B.attrName  $ show i ++ show j
                , (colors M.! i) `on` (colors M.! j) )
              | i <- [1..numColors], j <- [1..numColors] ]

colors :: M.Map Int B.Color
colors = let f :: Int -> B.Color
             f k = B.rgbColor k k k
  in M.fromList [ (1, B.red)
                , (2, B.green)
                , (3, B.blue)
                , (4, B.brightRed)
                , (5, B.brightGreen)
                , (6, B.brightBlue)
                , (7, B.rgbColor 1 0 0)
                , (8, B.rgbColor 0 1 0)
                , (9, B.rgbColor 0 0 1)
                ]

numColors :: Int
numColors = M.size colors
