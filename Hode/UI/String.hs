{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.String (
    resultsText  -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewExpr
  , showViewExprNode -- ^ ViewExprNode -> String
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
  p :: Maybe (Porest BufferRow)
  p = st ^? stGetFocusedBuffer . _Just . bufferRowPorest . _Just

  go :: Int -> PTree BufferRow -> [String]
  go i tv = indent (showViewExprNode $ tv ^. pTreeLabel . viewExprNode)
    : concatMap (go $ i+1) (maybe [] id $ toList <$> tv ^. pMTrees)
    where indent :: String -> String
          indent s = replicate (2*i) ' ' ++ s

resultView :: Rslt -> Addr -> Either String ViewExpr
resultView r a = do
  (s :: String) <- prefixLeft "resultView"
                   $ addrToExpr r a >>= eShow r
  Right $ ViewExpr { _viewResultAddr = a
                     , _viewResultString = s }

-- | `showViewExprNode` is used to display a `ViewExprNode` in the UI. It is distinct
-- from `show` so that `show` can show everything about the `ViewExprNode`,
-- whereas `showViewExprNode` hides things that the UI already makes clear.
showViewExprNode :: ViewExprNode -> String -- TODO : rename showViewExprNode
showViewExprNode (VQuery vq)  = vq
showViewExprNode (VExpr qr) = show (qr ^. viewResultAddr)
  ++ ": " ++ show (qr ^. viewResultString)
showViewExprNode (VMemberGroup _) = "its members"
showViewExprNode (VHostGroup (RelHostGroup x)) = show x
showViewExprNode (VHostGroup (TpltHostGroup x)) = show x
