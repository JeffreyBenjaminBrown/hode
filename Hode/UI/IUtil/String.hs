{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.IUtil.String (
    focusedBufferStrings  -- ^ St -> [String]
  , mkViewExpr   -- ^ Rslt -> Addr -> Either String ViewExpr
  , redraw_focusedBuffer -- ^ St -> Either String St
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
  p :: Maybe (Porest ExprRow)
  p = st ^? stGet_focusedBuffer . _Just .
      bufferExprRowTree . pMTrees . _Just

  go :: Int -> PTree ExprRow -> [String]
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

redraw_focusedBuffer :: St -> Either String St
redraw_focusedBuffer st =
  prefixLeft "redraw_focusedBuffer:" $ do
  let r :: Rslt = st ^. appRslt
      vo :: ViewOptions = st ^. viewOptions
  case st ^. stGet_focusedBuffer of
    Nothing -> Left "Focused buffer not found."
    Just b0 -> do
      b :: Buffer <- redraw_viewExpr_Strings r vo b0
      Right $ st & stSet_focusedBuffer .~ b

redraw_viewExpr_Strings
  :: Rslt -> ViewOptions -> Buffer -> Either String Buffer
redraw_viewExpr_Strings r vo b0 =
  prefixLeft "redraw_viewExpr_Strings:" $ let
  redrawPorest :: Porest ExprRow -> Either String (Porest ExprRow)
  redrawPorest = mapM redrawPTree
  redrawPTree :: PTree ExprRow -> Either String (PTree ExprRow)
  redrawPTree = mapM redrawSingle

  redrawSingle :: ExprRow -> Either String ExprRow
  redrawSingle br = case _viewExprNode br of
    VenExpr ve0 -> do
      ve <- mkViewExpr r vo
            (_viewExpr_showAsAddrs ve0)
            (_viewExpr_Addr ve0)
      Right $ br & viewExprNode .~ VenExpr ve
    _ -> Right br

  in case b0 ^. bufferExprRowTree . pMTrees of
       Nothing -> Right b0
       Just pbr0 -> do
         pbr :: Porest ExprRow <-
           redrawPorest pbr0
         Right $ b0 & bufferExprRowTree . pMTrees .~ Just pbr
