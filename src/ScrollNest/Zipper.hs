-- | Like ScrollNest, but with more convenient, faster types.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module ScrollNest.Zipper where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.Tree (Tree(Node))
import qualified Data.Tree as T
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
        , _windows = Z.modifyLabel (isFocused .~ True) wz }

app :: B.App St e Path
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

windowDraw :: Window -> B.Widget Path
windowDraw w =
  B.renderEditor (str . unlines) (w ^. isFocused) (w ^. windowEd)

treeDraw :: St -> Tree Window -> B.Widget Path
treeDraw st (Node w ws) =
  windowDraw w
  <=> padLeft (B.Pad 2) (vBox $ map (treeDraw st) ws)

appDraw :: St -> [B.Widget Path]
appDraw st = [treeDraw st $ Z.tree $ Z.root $ st ^. windows]

-- | Ignore the list; this app needs cursor locations to be in a tree (or
-- maybe a map, keys of which are first drawn from a tree in the `St`).
appChooseCursor ::
  St -> [B.CursorLocation Path] -> Maybe (B.CursorLocation Path)
appChooseCursor _ _ = Nothing

appHandleEvent ::
  St -> B.BrickEvent Path e -> B.EventM Path (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc []        -> B.halt st
  -- B.EvKey (B.KChar 'i') [] ->
  _ -> B.continue st
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr,        B.white `on` B.black)
    , (B.editFocusedAttr, B.black `on` B.yellow)
    ]

--stFocusWindow :: St -> Maybe Window
--stFocusWindow st = error "todo"
