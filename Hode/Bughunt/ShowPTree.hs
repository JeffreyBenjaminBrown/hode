{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Bughunt.ShowPTree where

import qualified Brick.Types          as B
import           Brick.Widgets.Core


showTwoAspects :: forall a b n.
     (b -> B.Widget n)
  -> (a -> b) -- ^ shows one aspect of an `a`
  -> (a -> b) -- ^ shows another
  -> [a]      -- ^ what to show
  -> B.Widget n
showTwoAspects b2w showColumns showNodes =
  vBox . map showRow
  where
    showRow :: a -> B.Widget n
    showRow a = hBox [ b2w $ showColumns a
                     , b2w $ showNodes a ]
