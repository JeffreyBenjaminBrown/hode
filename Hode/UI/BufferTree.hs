{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Hode.UI.BufferTree (
    consBuffer_topNext -- ^ Buffer -> St -> St
  , consBufferAsChild  -- ^ Buffer -> St -> St
  , cons_focusedViewExpr_asChildOfBuffer -- ^ St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> St
  ) where

import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import Hode.UI.ITypes
import Hode.UI.IUtil
import Hode.UI.Window
import Hode.Util.Direction
import Hode.Util.Misc
import Hode.Util.PTree


consBuffer_topNext :: Buffer -> St -> St
consBuffer_topNext b = searchBuffers . _Just %~ cons_topNext b

consBufferAsChild :: Buffer -> St -> St
consBufferAsChild b = searchBuffers . _Just . P.focus . setFocusedSubtree
                      %~ consUnderAndFocus (pTreeLeaf b)

cons_focusedViewExpr_asChildOfBuffer :: St -> Either String St
cons_focusedViewExpr_asChildOfBuffer st =
  prefixLeft "cons_focusedViewExpr_asChild" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocusedBuffer
  (ptrv :: PTree RsltView) <-
    let s = "getFocusedSubtree returned Nothing from bufferRsltViewPorest."
    in maybe (Left s) (maybe (Left s) Right) $
       b ^? bufferRsltViewPorest . _Just . P.focus . getFocusedSubtree
  b' <- bufferFromRsltViewTree ptrv
  Right $ st & hideReassurance & consBufferAsChild b'

moveFocusedBuffer :: Direction -> St -> St
moveFocusedBuffer d = searchBuffers . _Just %~ moveFocusInPorest d
