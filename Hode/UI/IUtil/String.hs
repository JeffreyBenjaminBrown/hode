{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil.String (
    focusedBufferStrings  -- ^ St -> [String]
  , mkViewExpr   -- ^ Rslt -> Addr -> Either String ViewExpr
  ) where

import           Data.Set (Set)
import           Data.Foldable (toList)
import           Lens.Micro

import Hode.Brick
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
  go i tbr = indent ( showBrief $
                      tbr ^. pTreeLabel . viewExprNode )
            : concatMap (go $ i+1)
            (maybe [] id $ toList <$> tbr ^. pMTrees)
    where indent :: String -> String
          indent s = replicate (2*i) ' ' ++ s

mkViewExpr :: Rslt -> ViewOptions -> Set Addr -> Addr
           -> Either String ViewExpr
mkViewExpr r vo as a =
  prefixLeft "mkViewExpr:" $ do
  s :: ColorString <-
    let as' = if _viewOpt_ShowAsAddresses vo
              then as else mempty
    in eParenShowColorAddr 3 r as' a
  Right $ ViewExpr { _viewExpr_Addr = a
                   , _viewExpr_showAsAddrs = as
                   , _viewExpr_String = s }
