{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    emptySt      -- ^ Rslt -> St
  , emptyBuffer  -- ^ Buffer
  , rsltViewLeaf -- ^ RsltView -> VTree RsltView
  , unEitherSt   -- ^ Either String St -> St -> St
  , resultsText  -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewResult
  , vShow        -- ^ RsltView -> String
  ) where

import qualified Data.Map                 as M
import qualified Data.Vector              as V
import           Lens.Micro

import qualified Brick.Focus              as B
import qualified Brick.Widgets.Edit       as B

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import UI.Window
import Util.Misc


emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _buffers = V.singleton
               $ VTree { _vTreeLabel = emptyBuffer
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

emptyBuffer :: Buffer
emptyBuffer = Buffer { _bufferQuery = ""
                     , _bufferView = rsltViewLeaf $ VQuery ""
                     , _bufferPath = [] }

rsltViewLeaf :: RsltView -> VTree RsltView
rsltViewLeaf v = VTree {
    _vTreeLabel = v
  , _vTreeFocus = 0
  , _vTreeIsFocused = False
  , _vTrees = V.empty }

unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old & showError s
unEitherSt _ (Right new) = new & showingInMainWindow .~ Results

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
