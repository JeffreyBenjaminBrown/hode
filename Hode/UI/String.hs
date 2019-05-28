{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.String (
    resultsText  -- ^ St -> [String]
  , resultView   -- ^ Rslt -> Addr -> Either String ViewExpr
  , show_ViewExprNode -- ^ ViewExprNode -> String
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
  p = st ^? stGetFocused_Buffer . _Just . bufferRowPorest . _Just

  go :: Int -> PTree BufferRow -> [String]
  go i tv = indent (show_ViewExprNode $ tv ^. pTreeLabel . viewExprNode)
    : concatMap (go $ i+1) (maybe [] id $ toList <$> tv ^. pMTrees)
    where indent :: String -> String
          indent s = replicate (2*i) ' ' ++ s

resultView :: Rslt -> Addr -> Either String ViewExpr
resultView r a = do
  (s :: String) <- prefixLeft "resultView"
                   $ addrToExpr r a >>= eShow r
  Right $ ViewExpr { _viewExpr_Addr = a
                     , _viewResult_String = s }

-- | `show_ViewExprNode` is used to display a `ViewExprNode` in the UI. It is distinct
-- from `show` so that `show` can show everything about the `ViewExprNode`,
-- whereas `show_ViewExprNode` hides things that the UI already makes clear.
show_ViewExprNode :: ViewExprNode -> String -- TODO : rename show_ViewExprNode
show_ViewExprNode (VQuery vq)  = vq
show_ViewExprNode (VExpr qr) = show (qr ^. viewExpr_Addr)
  ++ ": " ++ show (qr ^. viewResult_String)
show_ViewExprNode (VMemberGroup _) = "its members"
show_ViewExprNode (VHostGroup (RelHostGroup x)) = show x
show_ViewExprNode (VHostGroup (TpltHostGroup x)) = show x
