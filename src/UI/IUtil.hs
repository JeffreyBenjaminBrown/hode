{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    initialState -- ^ Rslt -> St
  , unEitherSt   -- ^ Either String St -> St -> St
  , emptyCommandWindow -- ^ St -> St
  , resultsText  -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewResult
  , viewLeaf     -- ^ View -> ViewTree
  , vShow        -- ^ View -> String
  ) where

import qualified Data.Vector as V
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Focus        as B
import qualified Brick.Widgets.Edit as B

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc


initialState :: Rslt -> St
initialState r = St {
    _focusRing = B.focusRing [Commands, Results]
      -- Almost always (for safety), Results is listed first. Not so
      -- here, because we want focus to start on the Commands window.
  , _viewTree  = ViewTree { _viewChildFocus = 0
                          , _viewIsFocused = False
                          , _viewContent = VQuery ""
                          , _viewSubviews = V.empty
                          }
  , _pathToFocus = []
  , _uiError   = ""
  , _commands  = B.editor Commands Nothing ""
  , _appRslt   = r
  , _shownInResultsWindow = ShowingResults
  }

unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old
  & shownInResultsWindow .~ ShowingError
  & uiError .~ s
unEitherSt _ (Right new) = new
  & shownInResultsWindow .~ ShowingResults

emptyCommandWindow :: St -> St
emptyCommandWindow = commands . B.editContentsL
                     .~ TxZ.textZipper [] Nothing

resultsText :: St -> [String]
resultsText st = f 0 $ st ^. viewTree where
  indent :: Int -> String -> String
  indent i s = replicate (2*i) ' ' ++ s

  f :: Int -> ViewTree -> [String]
  f i v = indent i (vShow $ v ^. viewContent)
    : concatMap (f $ i+1) (V.toList $ v ^. viewSubviews)

resultView :: Rslt -> Addr -> Either String ViewResult
resultView r a = do
  (s :: String) <- prefixLeft "resultView"
                   $ addrToExpr r a >>= eShow r
  Right $ ViewResult { _viewResultAddr = a
                     , _viewResultString = s }

viewLeaf :: View -> ViewTree
viewLeaf v = ViewTree {
    _viewChildFocus = 0
  , _viewIsFocused = False
  , _viewContent = v
  , _viewSubviews = V.empty }

-- | `vShow` is used to display a `View` in the UI. It is distinct
-- from `show` so that `show` can show everything about the `View`,
-- whereas `vShow` hides things that are already clear in the UI context.
vShow :: View -> String
vShow (VQuery vq)  = vq
vShow (VResult qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)
vShow (VMembers a) = "memebers of Expr at Addr "
                     ++ show (a ^. mvCenter)
vShow (VCenterRole crv) = show crv
