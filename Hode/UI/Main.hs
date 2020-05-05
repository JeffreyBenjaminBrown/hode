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

import qualified Hode.Brick.Help       as H
import           Hode.Hash.Types
import           Hode.PTree.Initial
import           Hode.Qseq.Types (Var(..))
import           Hode.Rslt.Index (mkRslt)
import           Hode.Rslt.Types
import           Hode.UI.BufferShow
import           Hode.UI.Input.KeyMaps_and_Docs
import           Hode.UI.Input.Util
import           Hode.UI.Types.Names
import           Hode.UI.Types.State
import           Hode.UI.Util
import           Hode.UI.Window


ui :: IO St
ui = uiFromRslt $ mkRslt mempty

uiFromSt :: St -> IO St
uiFromSt = B.defaultMain app

uiFromRslt :: Rslt -> IO St
uiFromRslt = B.defaultMain app . emptySt

app :: B.App St e BrickName
app = B.App
  { B.appDraw = \st ->
      case st ^. mainWindow of
        HelpBuffer -> [ H.helpUi BrickHelpName $
                        st ^. stHelp ]
        _          -> appDraw st
  , B.appChooseCursor = appChooseCursor
  , B.appHandleEvent  = \st ->
      case st ^. mainWindow of
        HelpBuffer -> H.respond stHelp
                      (mainWindow .~ SubgraphBuffer) st
        _          -> appHandleEvent st
  , B.appStartEvent   = return
  , B.appAttrMap      = \st ->
      case st ^. mainWindow of
        HelpBuffer -> H.attrMap
        _          -> appAttrMap
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
      HelpBuffer     -> error "impossible -- this case was handled already, in the definition of Hode.UI.Main.app."

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
    , padTop (B.Pad 2) $ strWrap $ "(To escape this error message, pick another window. For help press M-h (on most systems that means Alt-h).)" ]

  reassuranceWindow :: B.Widget BrickName =
    withAttr (B.attrName "white on blue")
    $ strWrap $ st0 ^. reassurance

appChooseCursor :: St -> [B.CursorLocation BrickName]
                -> Maybe (B.CursorLocation BrickName)
appChooseCursor = B.focusRingCursor (^. focusRing)

appHandleEvent :: St -> B.BrickEvent BrickName e
               -> B.EventM BrickName (B.Next St)
appHandleEvent st (B.VtyEvent ev) =
  case M.lookup ev $ M.fromList $
       map keyCmd_usePair universal_keyCmds of
  Just c  -> c st
  Nothing -> case stMode st of
    SubgraphMode ->
      case M.lookup ev $ M.fromList $
           map keyCmd_usePair subgraphBuffer_universal_keyCmds
      of Just c -> c st
         _ -> case st ^. subgraphSubmode of
           SubgraphSubmode_primary ->
             case M.lookup ev $ M.fromList $
                  map keyCmd_usePair subgraphBuffer_primary_keyCmds
             of Just c -> c st
                _ -> B.continue st
           SubgraphSubmode_sort ->
             case M.lookup ev $ M.fromList $
                  map keyCmd_usePair subgraphBuffer_sort_keyCmds
             of Just c -> c st
                _ -> B.continue st
    BufferMode ->
      case M.lookup ev $ M.fromList $
           map keyCmd_usePair bufferBuffer_keyCmds
      of Just c -> c st
         _ -> B.continue st
    LangCmdMode ->
      case M.lookup ev $ M.fromList $
           map keyCmd_usePair commandWindow_keyCmds
      of Just c -> c st
         _ -> B.continue =<<
           case B.focusGetCurrent $ st ^. focusRing of
           Just (BrickOptionalName LangCmds) ->
             -- pipe user input into the LangCmds window
             B.handleEventLensed (hideReassurance st)
             commands B.handleEditorEvent ev
           _ -> return st
    _ -> B.continue st
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

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName LangCmds]
  , _searchBuffers   = Just ( porestLeaf emptySubgraphBuffer
                              & P.focus . pTreeHasFocus .~ True )
  , _columnHExprs    = [ HMemberHosts $ HVar VarRowNode ]
  , _blockingCycles  = Nothing
  , _uiError         = ""
  , _reassurance     = "Welcome to Hode! Press M-? (probably Alt-?, if your computer has an Alt key) for help."
  , _commands        = B.editor (BrickOptionalName LangCmds) Nothing ""
  , _commandHistory  = []
  , _appRslt         = r
  , _viewOptions     = defaulViewOptions
  , _mainWindow      = SubgraphBuffer
  , _optionalWindows = S.fromList [ LangCmds, Reassurance ]
  , _subgraphSubmode  = SubgraphSubmode_primary
  , _stHelp           = H.initState modes
  }
