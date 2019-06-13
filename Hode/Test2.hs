-- | Like the module Hode.Test, but rather than `attrStringWrap`
-- it uses a different, apparently entirely equivalent, function
-- `attrStringWrap'` (with an apostrophe).

module Hode.Test2 where

import           Control.DeepSeq (force)
import           Lens.Micro

import qualified Brick.Main   as B
import           Brick.Types
import qualified Graphics.Vty as V

import Hode.Lib
import Hode.Test


test_showTwoAspects' :: IO ()
test_showTwoAspects' = B.simpleMain ( showTwoAspects
  attrStringWrap' showRowCol showRowNode rows :: Widget () )

test_showOneAspect' :: IO ()
test_showOneAspect' = B.simpleMain ( showOneAspect
  attrStringWrap' showRowCol showRowNode rows :: Widget () )

-- | `attrStringWrap'` is based on `Brick.Widgets.Core.txtWrapWith`.
-- It behaves identically, as far as I can tell,
-- to `attrStringWrap` (without the apostrophe).
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
