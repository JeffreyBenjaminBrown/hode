-- | Like ScrollNest, but with more convenient, faster types.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module ScrollNest.Zipper where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.Tree (Tree(Node))
import qualified Data.Tree as T
import qualified Data.Text.Zipper as TextZ hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as TextZ
import           Data.Tree.Zipper (TreePos, Full)
import qualified Data.Tree.Zipper as Z
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
type Path = [Int]

type Ed = B.Editor String Path

data Window = Window { _isFocused :: Bool
                     , _windowEd :: Ed }
makeLenses ''Window

data St = St { _windows :: TreePos Full Window
             , _focus :: () } -- ^ will use later
makeLenses ''St

zLabel :: Lens' (TreePos Full a) a
zLabel = lens Z.label $ flip Z.setLabel


---- | Brick

main :: IO St
main = mainFrom aState

mainFrom :: St -> IO St
mainFrom = B.defaultMain app

aState :: St
aState = let
  (pw :: Path -> Window) = \p -> Window
    { _isFocused = False
    , _windowEd = B.editor p (Just 1) $ show p }

  pt :: Path -> Tree Int -> Tree Window
  pt p (Node i ns) = Node  (pw $ i : p)
                     $ map (pt $ i : p) ns

  (wz :: TreePos Full Window) = Z.fromTree $ pt [] $
    Node 0 [ Node 0 []
           , Node 1 [ Node 0 []
                    , Node 1 [] ] ]

  in St { _focus = ()
        , _windows = wz }

app :: B.App St e Path
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

appDraw :: St -> [B.Widget Path]
appDraw st =
  [treeDraw $ Z.tree $ Z.root $ focusedSt ^. windows ]
  where
    focusedSt = st & windows . zLabel . isFocused .~ True

    treeDraw :: Tree Window -> B.Widget Path
    treeDraw (Node w0 ws) =
      windowDraw w0
      <=> padLeft (B.Pad 2) (vBox $ map treeDraw ws)

      where
        windowDraw :: Window -> B.Widget Path
        windowDraw w =
          B.renderEditor (str . unlines)
          (w ^. isFocused) (w ^. windowEd)

-- | Ignore the list; this app needs cursor locations to be in a tree (or
-- maybe a map, keys of which are first drawn from a tree in the `St`).
appChooseCursor ::
  St -> [B.CursorLocation Path] -> Maybe (B.CursorLocation Path)
appChooseCursor st _ = let
  e = st ^. windows . zLabel . windowEd
  z = e ^. B.editContentsL
  cp = TextZ.cursorPosition z
  toLeft = TextZ.take (cp ^. _2) (TextZ.currentLine z)
  (cursorLoc :: B.Location) = B.Location (textWidth toLeft, cp^._1)
  (n :: Path) = getName e
  in Just $ B.CursorLocation cursorLoc $ Just n

appHandleEvent ::
  St -> B.BrickEvent Path e -> B.EventM Path (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc [] -> B.halt st
  B.EvKey (B.KChar 'l') [B.MCtrl] ->
    B.continue $ st & windows %~ \w -> maybe w id $ Z.firstChild w
  B.EvKey (B.KChar 'k') [B.MCtrl] ->
    B.continue $ st & windows %~ \w -> maybe w id $ Z.next w
  B.EvKey (B.KChar 'i') [B.MCtrl] ->
    B.continue $ st & windows %~ \w -> maybe w id $ Z.prev w
  B.EvKey (B.KChar 'j') [B.MCtrl] ->
    B.continue $ st & windows %~ \w -> maybe w id $ Z.parent w
  _ -> B.continue =<< B.handleEventLensed st
   (windows . zLabel . windowEd) B.handleEditorEvent ev
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr,        B.white `on` B.black)
    , (B.editFocusedAttr, B.black `on` B.yellow)
    ]
