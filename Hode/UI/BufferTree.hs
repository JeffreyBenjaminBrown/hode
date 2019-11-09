{-# LANGUAGE
ScopedTypeVariables,
ViewPatterns
#-}

module Hode.UI.BufferTree
  ( cons_focusedViewExpr_asChildOfBuffer
                       -- ^ St -> Either String St
  , moveFocusedBuffer  -- ^ Direction -> St -> St
  , consBuffer_topNext -- ^ Buffer    -> St -> St
  , consBuffer_asChild -- ^ Buffer    -> St -> St
  ) where

import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.IUtil
import Hode.UI.Window
import Hode.Util.Direction
import Hode.Util.Misc
import Hode.Util.PTree


cons_focusedViewExpr_asChildOfBuffer :: St -> Either String St
cons_focusedViewExpr_asChildOfBuffer st =
  prefixLeft "cons_focusedViewExpr_asChild" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocused_Buffer
  (ptbr :: PTree BufferRow) <-
    maybe
    (Left "bufferRowPorest or focusedSubtree not found.")
    Right $ b ^? ( bufferRowPorest . _Just . P.focus
                   . getFocusedSubtree . _Just )
  b' :: Buffer <- buffer_from_bufferRowTree ptbr
  Right $ st & hideReassurance & consBuffer_asChild b'

moveFocusedBuffer :: Direction -> St -> St
moveFocusedBuffer d =
  searchBuffers . _Just %~ moveFocusInPorest d

consBuffer_topNext :: Buffer -> St -> St
consBuffer_topNext b =
  searchBuffers . _Just %~ cons_topNext b

consBuffer_asChild :: Buffer -> St -> St
consBuffer_asChild b =
  searchBuffers . _Just . P.focus . setFocusedSubtree
  %~ consUnder_andFocus (pTreeLeaf b)
