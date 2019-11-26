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

redraw_viewExpr_Strings
  :: Rslt -> ViewOptions -> Buffer -> Either String Buffer
redraw_viewExpr_Strings r vo b0 = let
  redrawPorest :: Porest BufferRow -> Either String (Porest BufferRow)
  redrawPorest = mapM redrawPTree
  redrawPTree :: PTree BufferRow -> Either String (PTree BufferRow)
  redrawPTree = mapM redrawSingle

  redrawSingle :: BufferRow -> Either String BufferRow
  redrawSingle br = case _viewExprNode br of
    VExpr ve0 -> do
      ve <- mkViewExpr r vo
            (_viewExpr_showAsAddrs ve0)
            (_viewExpr_Addr ve0)
      Right $ br & viewExprNode .~ VExpr ve
    _ -> Right br

  in case _bufferRowPorest b0 of
       Nothing -> Right b0
       Just pbr0 -> do
         pbr :: Porest BufferRow <-
           redrawPorest pbr0
         Right $ b0 { _bufferRowPorest = Just pbr }
