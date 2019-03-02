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


-- | = Types

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


-- | Brick

main :: IO St
main = mainFrom aState

mainFrom :: St -> IO St
mainFrom = B.defaultMain app

aState :: St
aState = let
  pw :: Path -> Window
  pw p = Window { _windowPath = p
                , _windowEditor =
                  B.editor p (Just 1) $ show p }

  pt :: Path -> Tree Int -> Tree Window
  pt p (Tree i ns) = let p' = V.cons i p
                     in    Tree (pw p')
                        $ V.map (pt p') ns

  vfl = V.fromList

  in St { _focus = V.fromList [0,1]
        , _windows = pt V.empty $
          Tree 0 $ vfl [ Tree 0 $ vfl []
                       , Tree 1 $ vfl [ Tree 0 $ vfl []
                                      , Tree 1 $ vfl [] ] ] }

app :: B.App St e Path
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

treeDraw :: St -> Tree Window -> B.Widget Path
treeDraw st (Tree (Window p e) ws) =
  B.renderEditor (str . unlines) (V.reverse p == st ^. focus) e
  <=> padLeft (B.Pad 2) (vBox $ map (treeDraw st) $ V.toList ws)

appDraw :: St -> [B.Widget Path]
appDraw st = [treeDraw st $ st ^. windows]

-- | Ignore the list; this app needs cursor locations to be in a tree (or
-- maybe a map, keys of which are first drawn from a tree in the `St`).
appChooseCursor ::
  St -> [B.CursorLocation Path] -> Maybe (B.CursorLocation Path)
appChooseCursor _ _ = Nothing

appHandleEvent ::
  St -> B.BrickEvent Path e -> B.EventM Path (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc []        -> B.halt st
  B.EvKey (B.KChar 'i') [] -> B.continue $ st & focus .~ V.singleton 0
  _ -> B.continue st
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr,        B.white `on` B.black)
    , (B.editFocusedAttr, B.black `on` B.yellow)
    ]

stFocusWindow :: St -> Maybe Window
stFocusWindow st = error "todo"
