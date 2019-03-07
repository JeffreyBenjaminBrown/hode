-- | Brick's builtin focus mechanism is deisgned to select among
-- different `Named` `Widget`s. The only way to name a `strWrap` that
-- I see is to wrap it in something like a Brick.Widgets.List, which
-- feels heavyweight. This is an alternative.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Experim.MyFocus where

import Brick
import Graphics.Vty


data BoxName = BoxName Int deriving (Show, Eq, Ord)
data Box = Box { boxName :: BoxName
               , boxContent :: String } deriving (Show, Eq, Ord)
data Boxes = Boxes { boxes :: [Box]
                   , boxFocus :: BoxName } deriving (Show, Eq, Ord)

aBoxes :: Boxes
aBoxes = Boxes { boxes = bs, boxFocus = BoxName 3 }
  where bs = [ Box { boxName = BoxName i
                   , boxContent = concat $ replicate 40 $ show i ++ " " }
             | i <- [1..10] ]

boxAttrMap :: AttrMap
boxAttrMap = attrMap
             ( white `on` blue ) -- the default
             [ ("focused", white `on` green) ]
  
drawBoxes :: Boxes -> Widget ()
drawBoxes bs = vBox $ map drawBox $ boxes bs where
  drawBox :: Box -> Widget ()
  drawBox b = style $ strWrap $ boxContent b where
    style :: Widget () -> Widget ()
    style = case boxFocus bs == boxName b of
      True -> withAttr "focused"
      False -> id

app :: App () e ()
app = App { appDraw = const [drawBoxes aBoxes]
          , appHandleEvent = resizeOrQuit
          , appStartEvent = return
          , appAttrMap = const boxAttrMap
          , appChooseCursor = neverShowCursor
          }

main :: IO ()
main = defaultMain app ()
