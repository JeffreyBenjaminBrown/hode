{-# LANGUAGE OverloadedStrings
, ScopedTypeVariables
, TupleSections #-}

module Hode.UI.Main where

import           Control.Lens
import qualified Data.List.PointedList as P
import qualified Data.Set              as S
import qualified Data.Map              as M
import           Data.Maybe

import qualified Brick.Main           as B
import qualified Brick.Types          as B
import           Brick.Widgets.Core   as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit   as B
import qualified Brick.AttrMap        as B
import qualified Brick.Focus          as B
import           Brick.Util           as B
import qualified Graphics.Vty         as V

import Hode.PTree.Initial
import Hode.Rslt.Index (mkRslt)
import Hode.Rslt.Types
import Hode.UI.BufferShow
import Hode.UI.Util
import Hode.UI.Input
import Hode.UI.Types.Names
import Hode.UI.Types.State


ui :: IO St
ui = uiFromRslt $ mkRslt mempty

uiFromSt :: St -> IO St
uiFromSt = B.defaultMain app

uiFromRslt :: Rslt -> IO St
uiFromRslt = B.defaultMain app . emptySt

app :: B.App St e BrickName
app = B.App
  { B.appDraw         = appDraw
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = appHandleEvent
  , B.appStartEvent   = return
  , B.appAttrMap      = const appAttrMap
  }


-- | The focused subview is recalculated at each call to `appDisplay`.
-- Each `ViewExprNodeTree`'s `viewIsFocused` field is `False` outside of `appDisplay`.
appDraw :: St -> [B.Widget BrickName]
appDraw st0 = [w] where
  w :: B.Widget BrickName =
    B.center $
    ( if isJust $ st0 ^. optionalWindows . at Error
      then errorWindow else mainWindowUi )
    <=> optionalWindowUis

  st :: St = st0
    & ( searchBuffers . _Just . P.focus
        . setFocusedSubtree
        -- set focus in the BufferBuffer window
        . pTreeHasFocus .~ True )
    & ( stSetFocused_ViewExprNode_Tree
        -- set focus in the SubgraphBuffer window
        . pTreeHasFocus .~ True )

  b :: Buffer = maybe
    (error "Focused Buffer not found.") id
    $ st0 ^? stGet_focusedBuffer

  mainWindowUi :: B.Widget BrickName =
    case st ^. mainWindow of
      LangCmdHistory -> commandHistoryWindow
      BufferBuffer   -> bufferWindow $ st ^. searchBuffers
      SubgraphBuffer -> resultWindow (st ^. viewOptions)
                        (b ^. bufferExprRowTree . pMTrees)

  optionalWindowUis :: B.Widget BrickName =
    mShow Reassurance reassuranceWindow <=>
    mShow LangCmds commandWindow
    where mShow wName window =
            if S.member wName $ st ^. optionalWindows
            then window else emptyWidget

  commandHistoryWindow :: B.Widget BrickName =
    strWrap $ unlines $ map show $ st0 ^. commandHistory

  commandWindow :: B.Widget BrickName =
    vLimit 2
    ( B.withFocusRing (st ^. focusRing)
      (B.renderEditor $ str . unlines) (st ^. commands) )

  errorWindow :: B.Widget BrickName = vBox
    [ strWrap $ st ^. uiError
    , padTop (B.Pad 2) $ strWrap $ "(To escape this error message, press M-S-r (to go to SubgraphBuffer), M-S-b (BufferBuffer), or M-S-h (command History)." ]

  reassuranceWindow :: B.Widget BrickName =
    withAttr (B.attrName "white on blue")
    $ strWrap $ st0 ^. reassurance

appChooseCursor :: St -> [B.CursorLocation BrickName]
                -> Maybe (B.CursorLocation BrickName)
appChooseCursor = B.focusRingCursor (^. focusRing)

appHandleEvent :: St -> B.BrickEvent BrickName e
               -> B.EventM BrickName (B.Next St)
appHandleEvent st (B.VtyEvent ev) =
  case M.lookup ev $ universal_keyCmds_map of
  Just c -> c st
  Nothing -> case stMode st of
    SubgraphMode -> handleKeyboard_atSubgraphBuffer st ev
    BufferMode   -> handleKeyboard_atBufferBuffer  st ev
    LangCmdMode  -> handleKeyboard_atCommandWindow st ev
    _            -> B.continue st
appHandleEvent st _ = B.continue st

appAttrMap :: B.AttrMap
appAttrMap = let
  rc :: Int -> Int -> Int -> V.Color
    = V.rgbColor
  gray (k :: Int) = V.rgbColor k k k
  -- black  = gray 0
  -- gray1  = gray 1 -- PITFALL: Vty offers darker non-black grays.
  --  -- See VTY issue https://github.com/jtdaugherty/vty/issues/172
  white     = gray 255
  darkBlue  = rc 0 0 1
  darkGreen = rc 0 1 0
  darkRed   = rc 1 0 0
  in B.attrMap V.defAttr
    [ (B.editAttr                    , V.black `on` V.red) -- unused
    , (B.editFocusedAttr             , white   `on` darkBlue)
    , (B.attrName "white on red"     , white   `on` darkRed)
    , (B.attrName "white on green"   , white   `on` darkGreen)
    , (B.attrName "white on blue"    , white   `on` darkBlue)
    ]
