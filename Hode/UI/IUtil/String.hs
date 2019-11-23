{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil.String (
    focusedBufferStrings  -- ^ St -> [String]
  , mkViewExpr   -- ^ Rslt -> Addr -> Either String ViewExpr
  ) where

import           Data.Foldable (toList)
import           Lens.Micro

import Hode.Brick
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.ShowColor
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.Util.Misc
import Hode.PTree.Initial


-- | Render an entire `Buffer` to text.
focusedBufferStrings :: St -> [String]
focusedBufferStrings st =
  maybe [] (concatMap $ go 0) p
  where
  p :: Maybe (Porest BufferRow)
  p = st ^? stGetFocused_Buffer . _Just .
      bufferRowPorest . _Just

  go :: Int -> PTree BufferRow -> [String]
  go i tv = indent ( showBrief $
                     tv ^. pTreeLabel . viewExprNode )
            : concatMap (go $ i+1)
            (maybe [] id $ toList <$> tv ^. pMTrees)
    where indent :: String -> String
          indent s = replicate (2*i) ' ' ++ s

mkViewExpr :: Rslt -> Addr -> Either String ViewExpr
mkViewExpr r a = do
  (s :: ColorString) <- prefixLeft "mkViewExpr:"
    $ addrToExpr r a >>= eParenShowColorExpr 3 r
  Right $ ViewExpr { _viewExpr_Addr = a
                   , _viewExpr_String = s }

