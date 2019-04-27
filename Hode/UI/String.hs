{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.String (
    resultsText  -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewResult
  , showRsltView -- ^ RsltView -> String
  ) where

import           Data.Foldable (toList)
import           Lens.Micro

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.Show
import Hode.UI.ITypes
import Hode.Util.Misc
import Hode.Util.PTree


resultsText :: St -> [String]
resultsText st = maybe [] (concatMap $ go 0) p where
  p :: Maybe (Porest RsltView)
  p = st ^? stGetFocusedBuffer . _Just . bufferRsltViewPorest . _Just

  go :: Int -> PTree RsltView -> [String]
  go i tv = indent (showRsltView $ tv ^. pTreeLabel)
    : concatMap (go $ i+1) (maybe [] id $ toList <$> tv ^. pMTrees)
    where indent :: String -> String
          indent s = replicate (2*i) ' ' ++ s

resultView :: Rslt -> Addr -> Either String ViewResult
resultView r a = do
  (s :: String) <- prefixLeft "resultView"
                   $ addrToExpr r a >>= eShow r
  Right $ ViewResult { _viewResultAddr = a
                     , _viewResultString = s }

-- | `showRsltView` is used to display a `RsltView` in the UI. It is distinct
-- from `show` so that `show` can show everything about the `RsltView`,
-- whereas `showRsltView` hides things that the UI already makes clear.
showRsltView :: RsltView -> String -- TODO : rename showRsltView
showRsltView (VQuery vq)  = vq
showRsltView (VResult qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)
showRsltView (VMembers _) = "its members"
showRsltView (VHostGroup (RelHostGroup x)) = show x
showRsltView (VHostGroup (TpltHostGroup x)) = show x
