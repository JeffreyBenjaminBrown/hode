-- | Based on the demos in the programs/ folder of Brick,
-- particularly `EditDemo.hs`.

{-# LANGUAGE ScopedTypeVariables #-}

module UI.ScrollNest where

import           Control.Monad.IO.Class (liftIO)
import           Lens.Micro

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
type Name = [Int] 

data FocusTree = FocusLeaf | FocusTree Name [FocusTree]

data St = St {
    _focusTree :: FocusTree
    }

main :: IO St
main = B.defaultMain app initialState

initialState :: St
initialState = St $ FocusTree [] []

app :: B.App St e Name
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }

appDraw :: St -> [B.Widget n]
appDraw = error "?"

-- | Ignore the list; this app needs cursor locations to be in a tree (or
-- maybe a map, they keys of which come from some tree field of the `St`).
appChooseCursor ::
  St -> [B.CursorLocation Name] -> Maybe (B.CursorLocation Name)
appChooseCursor = error "?"

appHandleEvent ::
  St -> B.BrickEvent Name e -> B.EventM Name (B.Next St)
appHandleEvent = error "?"

appAttrMap :: B.AttrMap
appAttrMap = B.attrMap B.defAttr
    [ (B.editAttr,        B.white `on` B.blue)
    , (B.editFocusedAttr, B.black `on` B.yellow)
    ]
