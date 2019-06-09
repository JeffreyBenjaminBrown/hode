-- | I'm not sure this idea makes sense: If there are nested
-- scroll windows, how big should each one be?

-- | Run `main` and the use `Ctrl-[i,j,k,l]` to move around.
-- Press `Shift` to move faster vertically.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Experim.ScrollNest (scrollNest_main) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.Tree (Tree(Node))
import qualified Data.Text.Zipper as TxZ hiding ( textZipper )
import           Data.Tree.Zipper (TreePos, Full)
import qualified Data.Tree.Zipper as Z
import           Lens.Micro
import           Lens.Micro.TH

import qualified Brick.Main as B
import qualified Brick.Types as B
import           Brick.Widgets.Core
import qualified Brick.Widgets.Edit as B
import qualified Brick.AttrMap as B
import           Brick.Util (on)
import qualified Graphics.Vty as B

import UI.Clipboard (toClipboard)


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
             , _focus :: Name } -- TODO ? Is this useful?
  deriving (Show)
makeLenses ''St


-- | Type helpers

zLabel :: Lens' (TreePos Full a) a
zLabel = lens Z.label $ flip Z.setLabel

moveFocus :: (TreePos Full Window -> TreePos Full Window)
          -> ( St                 -> St)
moveFocus move = reFocus . (windows %~ move) . unFocus
  where unFocus, reFocus :: St -> St
        unFocus = windows . zLabel . isFocused .~ False
        reFocus st = st & focus .~ getName e
                     & windows . zLabel . isFocused .~ True
          where e = st ^. windows . zLabel . windowEd

firstFull :: TreePos Full a -> TreePos Full a
firstFull tz = maybe (error "impossible") id
               $ Z.nextTree $ Z.first $ Z.prevSpace tz

lastFull :: TreePos Full a -> TreePos Full a
lastFull tz = maybe (error "impossible") id
              $ Z.prevTree $ Z.last $ Z.prevSpace tz


-- | Brick

scrollNest_main :: IO St
scrollNest_main = scrollNest_mainFrom aState

scrollNest_mainFrom :: St -> IO St
scrollNest_mainFrom = B.defaultMain app

aState :: St
aState = let
  (pw :: Path -> Window) = \p -> Window
    { _isFocused = False
    , _windowEd = B.editor (ToSelf p) (Just 1) $ show p }

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

-- | Based on `Brick.Focus.focusRingCursor`
appChooseCursor :: St
                -> [B.CursorLocation Name]
                -> Maybe (B.CursorLocation Name)
appChooseCursor st =
  listToMaybe . filter isCurrent
  where isCurrent cl = cl ^. B.cursorLocationNameL
                       == Just (st ^. focus)

appHandleEvent ::
  St -> B.BrickEvent Name e -> B.EventM Name (B.Next St)
appHandleEvent st (B.VtyEvent ev) = case ev of
  B.EvKey B.KEsc [] -> B.halt st

  -- moving around
  B.EvKey (B.KChar 'l') [B.MMeta] ->
    B.continue $ st & moveFocus (\w -> maybe w id $ Z.firstChild w)
  B.EvKey (B.KChar 'k') [B.MMeta] ->
    B.continue $ st & moveFocus (\w -> maybe w id $ Z.next w)
  B.EvKey (B.KChar 'K') [B.MMeta] ->
    B.continue $ st & moveFocus lastFull
  B.EvKey (B.KChar 'i') [B.MMeta] ->
    B.continue $ st & moveFocus (\w -> maybe w id $ Z.prev w)
  B.EvKey (B.KChar 'I') [B.MMeta] ->
    B.continue $ st & moveFocus firstFull
  B.EvKey (B.KChar 'j') [B.MMeta] ->
    B.continue $ st & moveFocus (\w -> maybe w id $ Z.parent w)

  -- report extent of focused editor
  B.EvKey (B.KChar 'e') [B.MMeta] -> do
    let (n :: Name) = getName $ st ^. windows . zLabel . windowEd
    (s :: String) <- maybe "lookupExtent returned Nothing" show
                     <$> B.lookupExtent n
    B.continue $ replaceText s st

  -- report whether focused editor has focus (should be `True`)
  B.EvKey (B.KChar 'f') [B.MMeta] ->
    let (focused :: Bool) = st ^. windows . zLabel . isFocused
    in B.continue $ replaceText (show focused) st

  -- copy focused editor's content to clipboard
  B.EvKey (B.KChar 'c') [B.MMeta] ->
    liftIO ( toClipboard $ show $ TxZ.getText
             $ st ^. windows . zLabel . windowEd . B.editContentsL )
    >> B.continue st

  _ -> B.continue =<< B.handleEventLensed st
   (windows . zLabel . windowEd) B.handleEditorEvent ev
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
  [ (B.editAttr,        B.white `on` B.black)
  , (B.editFocusedAttr, B.black `on` B.yellow)
  ]

replaceText :: String -> St -> St
replaceText s =
  windows . zLabel . windowEd
  %~ B.applyEdit (TxZ.insertMany s)
