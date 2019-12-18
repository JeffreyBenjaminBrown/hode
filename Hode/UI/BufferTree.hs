{-# LANGUAGE
ScopedTypeVariables,
ViewPatterns
#-}

module Hode.UI.BufferTree
  ( cons_focusedViewExpr_asChildOfBuffer
                       -- ^ St -> Either String St
  , moveFocus_inBufferTree  -- ^ Direction -> St -> St
  , consBuffer_topNext -- ^ Buffer    -> St -> St
  , consBuffer_asChild -- ^ Buffer    -> St -> St
  ) where

import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.IUtil
import Hode.UI.IUtil.String
import Hode.UI.Window
import Hode.Util.Misc
import Hode.PTree.Initial


cons_focusedViewExpr_asChildOfBuffer :: St -> Either String St
cons_focusedViewExpr_asChildOfBuffer st0 =
  prefixLeft "cons_focusedViewExpr_asChild:" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st0 ^. stGetFocused_Buffer
  (ptbr :: PTree BufferRow) <-
    maybe
    (Left "bufferRowPorest or focusedSubtree not found.")
    Right $ b ^? ( bufferRowPorest . _Just . P.focus
                   . getFocusedSubtree . _Just )
  b' :: Buffer <- let
    f (VExpr ve) = VExpr $ ve { _viewExpr_showAsAddrs = mempty }
    f x = x -- should not happen
    in buffer_from_bufferRowTree
       $ ptbr & pTreeLabel . viewExprNode %~ f
  redraw_focusedBuffer $
    hideReassurance . consBuffer_asChild b' $ st0

moveFocus_inBufferTree :: Direction -> St -> St
moveFocus_inBufferTree d =
  searchBuffers . _Just %~ moveFocusInPorest d

consBuffer_topNext :: Buffer -> St -> St
consBuffer_topNext b =
  searchBuffers . _Just %~ cons_topNext b

consBuffer_asChild :: Buffer -> St -> St
consBuffer_asChild b =
  searchBuffers . _Just . P.focus . setFocusedSubtree
  %~ consUnder_andFocus (pTreeLeaf b)
