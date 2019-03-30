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
consBuffer_topNext b = buffers %~ cons_topNext b

consBufferAsChild :: Buffer -> St -> St
consBufferAsChild b st = st & buffers . P.focus . setFocusedSubtree
                            %~ consUnderAndFocus (pTreeLeaf b)

cons_focusedViewResult_asChildOfBuffer :: St -> Either String St
cons_focusedViewResult_asChildOfBuffer st =
  prefixLeft "cons_focusedViewResult_asChild" $ do
  p <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocusedBuffer
  (pt :: PTree RsltView) <-
    let s = "getFocusedSubtree returned Nothing from bufferRsltViewTree."
    in maybe (Left s) Right $ (p^.bufferRsltViewTree) ^. getFocusedSubtree
  p' <- bufferFromRsltViewTree pt
  Right $ st & hideReassurance & consBufferAsChild p'

moveFocusedBuffer :: Direction -> St -> St
moveFocusedBuffer d = buffers %~ moveFocusInPorest d
