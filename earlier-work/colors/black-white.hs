module Hode.UI.Main where

import           Brick.Util (on)
import           Brick.Widgets.Core
import qualified Brick.AttrMap        as B
import qualified Brick.Main           as B
import qualified Brick.Types          as B
import qualified Graphics.Vty         as B


main :: IO ()
main = B.defaultMain app ()

app :: B.App () e ()
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent  = \_ _ -> B.halt ()
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

appDraw :: () -> [B.Widget ()]
appDraw _ =
  [vBox [ withAttr (B.attrName i) $ str i
        | i <- map fst someGrays ]]

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr $
             map (\(x,y) -> (B.attrName x, y)) someGrays

-- | A logarithmic sampling of the color space.
-- Maybe I should be sampling linearly.
someGrays :: [ (String, B.Attr) ]
someGrays =
  let gray :: Int -> B.Color
      gray k = B.rgbColor k k k
  in

  -- Vty's white seems to be around gray 180, with slight variation
  -- between Konsole and Terminator.
  -- If I use Terminator, Vty's black is not as black as can be.
  [ ( "#### This is gray 160.",      gray 160 `on` gray 0 )
  , ( "#### This is Vty's white.",   B.white  `on` B.black )
  , ( "#### This is gray 200.",      gray 200 `on` gray 0 ) ]

  ++

  -- A roughly linear sampling of the color space,
  -- shifted by 6 to show that values 18 and 58 are weird.
  [ ("#### This is gray " ++ show i, gray i   `on` gray 0)
  | j <- [1..32]
  , let i = max 0 $ j*8-6 ]
