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
  v = vBox [ h j | j <- [1..8] ]
    where
    h j = hBox [ withAttr (B.attrName $ show i ++ show j) $
                 str $ show i ++ "#obj" ++ show j
               | i <- [1..8], i /= 1 ]

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr preMap1

  where
  preMap1 :: [ (B.AttrName, B.Attr) ]
  preMap1 = [ ( B.attrName  $ show i ++ show j
              , (preMap0 M.! i) `on` (preMap0 M.! j) )
            | i <- [1..8], j <- [1..8] ]

    where
    preMap0 :: M.Map Int B.Color
    preMap0 = M.fromList [ (1 , B.black)
                         , (2 , B.red)
                         , (3 , B.green)
                         , (4 , B.yellow)
                         , (5 , B.blue)
                         , (6 , B.magenta)
                         , (7 , B.cyan)
                         , (8 , B.white) ]
