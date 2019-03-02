-- | Like ScrollNest, but with more convenient, faster types.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.ScrollNest2 where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Main as B
import qualified Brick.Types as B
import           Brick.Widgets.Core
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.AttrMap as B
import qualified Brick.Focus as B
import           Brick.Util (on)
import qualified Graphics.Vty as B


-- | A path from the top window to the given window
-- The top window has the name [].
type Path = Vector Int

data Tree a = Tree { _load :: a
                   , _rest :: Vector (Tree a) }
makeLenses ''Tree

data Window = Window { _windowPath :: Path -- ^ reversed, for speed; the
  -- first digit in this `Path` is the last choice on the way to the window. 
  , _windowEditor :: B.Editor String Path }
makeLenses ''Window

data St = St { _windows :: Tree Window
             , _focus :: Path }
makeLenses ''St

fromTree :: forall a. Path -> Tree a -> Maybe a
fromTree v t = case null v of
  True -> Just $ _load t
  False -> maybe Nothing f next where
    (next :: Maybe (Tree a)) = (V.!?) (_rest t) (V.head v)
    (f :: Tree a -> Maybe a) = fromTree $ V.tail v

treeDraw :: St -> Tree Window -> B.Widget Path
treeDraw st (Tree (Window p e) ws) =
  B.renderEditor (str . unlines) (V.reverse p == st ^. focus) e
  <=> padLeft (B.Pad 2) (vBox $ map (treeDraw st) $ V.toList ws)
