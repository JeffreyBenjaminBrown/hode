-- | Run `main` and the use `Ctrl-[i,j,k,l]` to move around.
-- Press `Shift` to move faster vertically.

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
data Name = ToSelf {namePath :: Path}
          | ToChildren {namePath :: Path}
          deriving (Show, Eq, Ord)

data Window = Window { _isFocused :: Bool
                     , _windowEd :: B.Editor String Name }
  deriving (Show)
makeLenses ''Window

data St = St { _windows :: TreePos Full Window
             , _focus :: Name } -- TODO ? unused
  deriving (Show)
makeLenses ''St


-- | Type helpers

zLabel :: Lens' (TreePos Full a) a
zLabel = lens Z.label $ flip Z.setLabel

-- TODO ? Is this pointless
reFocus :: St -> St
reFocus st = -- TODO ? update the focus state of the focused window too
             -- (currently that's done in appDraw).
  let e = st ^. windows . zLabel . windowEd
  in st & focus .~ getName e

firstFull :: TreePos Full a -> TreePos Full a
firstFull tz = maybe (error "impossible") id
               $ Z.nextTree $ Z.first $ Z.prevSpace tz

lastFull :: TreePos Full a -> TreePos Full a
lastFull tz = maybe (error "impossible") id
              $ Z.prevTree $ Z.last $ Z.prevSpace tz


-- | Brick

main :: IO St
main = mainFrom aState

mainFrom :: St -> IO St
mainFrom = B.defaultMain app

aState :: St
aState = let
  (pw :: Path -> Window) = \p -> Window
    { _isFocused = False
    , _windowEd = B.editor (ToSelf p) (Just 2) $ show p }

  pt :: Path -> Tree Int -> Tree Window
  pt p (Node i ns) = Node  (pw $ i : p)
                     $ map (pt $ i : p) ns

  (wz :: TreePos Full Window) = Z.fromTree $ pt [] aTree

  in St { _focus = ToSelf [0]
        -- TODO ? [0] dangerous, use something like head instead
        , _windows = wz }

aTree :: Tree Int
aTree = Node 0 [ Node 0 []
               , Node 1 []
               , Node 2 [ Node 0 []
                        , Node 1 []
                        , Node 2 [ Node 0 []
                                 , Node 1 []
                                 , Node 2 []
                                 , Node 3 []
                                 , Node 4 [] ]
                        , Node 3 []
                        , Node 4 [] ]
               , Node 3 []
               , Node 4 [] ]

app :: B.App St e Name
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

appDraw :: St -> [B.Widget Name]
appDraw st =
  [treeDraw $ Z.tree $ Z.root $ focusedSt ^. windows ]
  where
    (focusedSt :: St) = st & windows . zLabel . isFocused .~ True

    treeDraw :: Tree Window -> B.Widget Name
    treeDraw (Node w0 ws) =
      windowDraw w0
      <=> ( padLeft (B.Pad 2)
            $ ( if n0 /= topName then id else
                  viewport ( ToChildren [0] ) B.Vertical )
            $ vBox
            $ map treeDraw ws )
      where

        (n0 :: Name) = getName $ w0 ^. windowEd
        (topName :: Name) = ToSelf [0]

        windowDraw :: Window -> B.Widget Name
        windowDraw w = (if foc then visible else id)
                       $ B.renderEditor (str . unlines)
                       (w ^. isFocused) (w ^. windowEd)
          where foc = w ^. isFocused

-- | Ignore the list; this app needs cursor locations to be in a tree (or
-- maybe a map, keys of which are first drawn from a tree in the `St`).
--
-- Broken. Lifted with little understanding from Bricks.Widgets.Edit.
appChooseCursor ::
  St -> [B.CursorLocation Name] -> Maybe (B.CursorLocation Name)
appChooseCursor st _ = let
  e = st ^. windows . zLabel . windowEd
  z = e ^. B.editContentsL
  cp = TextZ.cursorPosition z
  toLeft = TextZ.take (cp ^. _2) (TextZ.currentLine z)
  (cursorLoc :: B.Location) = B.Location (textWidth toLeft, cp^._1)
  (n :: Name) = getName e
  in Just $ B.CursorLocation cursorLoc $ Just n

appHandleEvent ::
  St -> B.BrickEvent Name e -> B.EventM Name (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc [] -> B.halt st
  B.EvKey (B.KChar 'l') [B.MMeta] ->
    B.continue $ reFocus $ st & windows %~ \w -> maybe w id $ Z.firstChild w
  B.EvKey (B.KChar 'k') [B.MMeta] ->
    B.continue $ reFocus $ st & windows %~ \w -> maybe w id $ Z.next w
  B.EvKey (B.KChar 'K') [B.MMeta] ->
    B.continue $ reFocus $ st & windows %~ lastFull
  B.EvKey (B.KChar 'i') [B.MMeta] ->
    B.continue $ reFocus $ st & windows %~ \w -> maybe w id $ Z.prev w
  B.EvKey (B.KChar 'I') [B.MMeta] ->
    B.continue $ reFocus $ st & windows %~ firstFull
  B.EvKey (B.KChar 'j') [B.MMeta] ->
    B.continue $ reFocus $ st & windows %~ \w -> maybe w id $ Z.parent w
  _ -> B.continue =<< B.handleEventLensed st
   (windows . zLabel . windowEd) B.handleEditorEvent ev
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
  [ (B.editAttr,        B.white `on` B.black)
  , (B.editFocusedAttr, B.black `on` B.yellow)
  ]
