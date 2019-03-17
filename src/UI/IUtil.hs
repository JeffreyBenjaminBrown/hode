{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    resultsText -- St -> [String]
  , resultView -- Rslt -> Addr -> Either String ViewResult
  , viewLeaf -- View -> ViewTree
  , vShow -- View -> String
  ) where

import qualified Data.Vector as V
import Lens.Micro

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc


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
