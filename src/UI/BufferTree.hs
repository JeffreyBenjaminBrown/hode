{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.BufferTree (
    consBuffer_topNext -- ^ Buffer -> St -> St
  , consBufferAsChild  -- ^ Buffer -> St -> St
  , cons_focusedViewResult_asChildOfBuffer -- ^ St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> St
  ) where

import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import UI.ITypes
import UI.IUtil
import UI.Window
import Util.Direction
import Util.Misc
import Util.PTree


consBuffer_topNext :: Buffer -> St -> St
consBuffer_topNext b = searchBuffers %~ cons_topNext b

consBufferAsChild :: Buffer -> St -> St
consBufferAsChild b = searchBuffers . P.focus . setFocusedSubtree
                      %~ consUnderAndFocus (pTreeLeaf b)

cons_focusedViewResult_asChildOfBuffer :: St -> Either String St
cons_focusedViewResult_asChildOfBuffer st =
  prefixLeft "cons_focusedViewResult_asChild" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocusedBuffer
  (pt :: PTree RsltView) <-
    let s = "getFocusedSubtree returned Nothing from bufferRsltViewPorest."
    in maybe (Left s) (maybe (Left s) Right) $
       b ^? bufferRsltViewPorest . _Just . P.focus . getFocusedSubtree
  b' <- bufferFromRsltViewTree pt
  Right $ st & hideReassurance & consBufferAsChild b'

moveFocusedBuffer :: Direction -> St -> St
moveFocusedBuffer d = searchBuffers %~ moveFocusInPorest d
