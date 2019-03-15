{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    resultsText -- St -> [String]
  , resultView -- Rslt -> Addr -> Either String ResultView
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

resultView :: Rslt -> Addr -> Either String ResultView
resultView r a = do
  (s :: String) <- prefixLeft "resultView"
                   $ addrToExpr r a >>= eShow r
  Right $ ResultView { _viewResultAddr = a
                     , _viewResultString = s }

viewLeaf :: View -> ViewTree
viewLeaf v = ViewTree {
    _viewFocus = 0
  , _viewIsFocused = False
  , _viewContent = v
  , _viewSubviews = V.empty }

vShow :: View -> String
vShow (VQuery vq)  = vq
vShow (VResult qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)
vShow (VCenterRoleView crv) = "View { " ++ show crv ++ " }"
