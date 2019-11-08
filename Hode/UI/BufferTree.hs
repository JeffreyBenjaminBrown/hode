{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Hode.UI.BufferTree
  ( consBuffer_topNext -- ^ Buffer -> St -> St
  , consBuffer_asChild -- ^ Buffer -> St -> St
  , cons_focusedViewExpr_asChildOfBuffer
                       -- ^ St -> Either String St
  , moveFocusedBuffer  -- ^ Direction -> St -> St
  ) where

import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import Hode.UI.ITypes.State
import Hode.UI.ITypes.Views
import Hode.UI.IUtil
import Hode.UI.Window
import Hode.Util.Direction
import Hode.Util.Misc
import Hode.Util.PTree


consBuffer_topNext :: Buffer -> St -> St
consBuffer_topNext b = searchBuffers . _Just %~ cons_topNext b

consBuffer_asChild :: Buffer -> St -> St
consBuffer_asChild b =
  searchBuffers . _Just . P.focus . setFocusedSubtree
  %~ consUnder_andFocus (pTreeLeaf b)

cons_focusedViewExpr_asChildOfBuffer :: St -> Either String St
cons_focusedViewExpr_asChildOfBuffer st =
  prefixLeft "cons_focusedViewExpr_asChild" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocused_Buffer
  (ptrv :: PTree BufferRow) <-
    let s = "getFocusedSubtree returned Nothing from bufferRowPorest."
    in maybe (Left s) (maybe (Left s) Right) $
       b ^? bufferRowPorest . _Just . P.focus . getFocusedSubtree
  b' <- buffer_from_bufferRowTree ptrv
  Right $ st & hideReassurance & consBuffer_asChild b'

moveFocusedBuffer :: Direction -> St -> St
moveFocusedBuffer d = searchBuffers . _Just %~ moveFocusInPorest d
