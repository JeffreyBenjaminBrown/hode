{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    initialState               -- ^ Rslt -> St
  , unEitherSt                 -- ^ Either String St -> St -> St
  , hideReassurance            -- ^           St -> St
  , showError, showReassurance -- ^ String -> St -> St
  , emptyCommandWindow         -- ^ St -> St
  , resultsText                -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewResult
  , viewLeaf     -- ^ RsltView -> VTree RsltView
  , vShow        -- ^ RsltView -> String
  ) where

import qualified Data.Map                 as M
import qualified Data.Vector              as V
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Focus              as B
import qualified Brick.Widgets.Edit       as B

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc


initialState :: Rslt -> St
initialState r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _buffers = let
      b = Buffer { _bufferQuery = ""
                 , _bufferView = VTree { _vTreeLabel = VQuery ""
                                       , _vTrees = V.empty
                                       , _vTreeFocus = 0
                                       , _vTreeIsFocused = False }
                 , _bufferPath = [] }
      in V.singleton $ VTree { _vTreeLabel = b
                             , _vTrees = V.empty
                             , _vTreeFocus = 0
                             , _vTreeIsFocused = False }
  , _vathToBuffer = (0,[])
  , _uiError   = ""
  , _reassurance = "It's all good."
  , _commands  = B.editor (BrickOptionalName Commands) Nothing ""
  , _commandHistory = []
  , _appRslt   = r
  , _showingInMainWindow = Results
  , _showingOptionalWindows = M.fromList [ (Commands   , True)
                                         , (Reassurance, True) ]
  }

hideReassurance :: St -> St
hideReassurance = showingOptionalWindows %~ M.insert Reassurance False

showError, showReassurance :: String -> St -> St
showError msg = (showingOptionalWindows %~ M.insert Reassurance False)
                . (showingInMainWindow .~ Errors)
                . (uiError .~ msg)
showReassurance msg = (showingOptionalWindows %~ M.insert Reassurance True)
                      . (reassurance .~ msg)

unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old & showError s
unEitherSt _ (Right new) = new & showingInMainWindow .~ Results

emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing

resultsText :: St -> [String]
resultsText st = maybe [] (f 0) b where
  b :: Maybe (VTree RsltView)
  b = st ^? stBuffer st . bufferView

  f :: Int -> VTree RsltView -> [String]
  f i v = indent (vShow $ v ^. vTreeLabel)
    : concatMap (f $ i+1) (V.toList $ v ^. vTrees)
    where indent :: String -> String
          indent s = replicate (2*i) ' ' ++ s

resultView :: Rslt -> Addr -> Either String ViewResult
resultView r a = do
  (s :: String) <- prefixLeft "resultView"
                   $ addrToExpr r a >>= eShow r
  Right $ ViewResult { _viewResultAddr = a
                     , _viewResultString = s }

viewLeaf :: RsltView -> VTree RsltView
viewLeaf v = VTree {
    _vTreeFocus = 0
  , _vTreeIsFocused = False
  , _vTreeLabel = v
  , _vTrees = V.empty }

-- | `vShow` is used to display a `RsltView` in the UI. It is distinct
-- from `show` so that `show` can show everything about the `RsltView`,
-- whereas `vShow` hides things that are already clear in the UI context.
vShow :: RsltView -> String
vShow (VQuery vq)  = vq
vShow (VResult qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)
vShow (VMembers a) = "memebers of Expr at Addr "
                     ++ show (a ^. mvCenter)
vShow (VCenterRole crv) = show crv
