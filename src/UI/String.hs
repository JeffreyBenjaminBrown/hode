{-# LANGUAGE ScopedTypeVariables #-}

module UI.String (
    resultsText_buffer -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewResult
  , vShow        -- ^ RsltView -> String
  ) where

import           Data.Foldable (toList)
import           Lens.Micro

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import Util.Misc
import Util.PTree


resultsText_buffer :: St -> [String]
resultsText_buffer st = maybe [] (go 0) p where
  p :: Maybe (PTree RsltView)
  p = st ^? stGetFocusedBuffer . _Just . bufferRsltViewTree

  go :: Int -> PTree RsltView -> [String]
  go i tv = indent (vShow $ tv ^. pTreeLabel)
    : concatMap (go $ i+1) (maybe [] id $ toList <$> tv ^. pMTrees)
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
vShow :: RsltView -> String -- TODO : rename vShow
vShow (VQuery vq)  = vq
vShow (VResult qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)
vShow (VMembers a) = "memebers of Expr at Addr "
                     ++ show (a ^. viewMembersCenter)
vShow (VCenterRole vcr) = show vcr
