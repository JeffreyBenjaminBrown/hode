{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.BufferTree (
    consBuffer_topNext -- ^ Buffer -> St -> St
  , consBufferAsChild  -- ^ Buffer -> St -> St
  , cons_focusedViewResult_asChildOfBuffer -- ^ St -> Either String St
  , debugging_cons_focusedViewResult_asChildOfBuffer
    -- ^ St -> Either String (Buffer, PTree RsltView, Buffer)
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
consBuffer_topNext b = searchBuffers . _Just %~ cons_topNext b

consBufferAsChild :: Buffer -> St -> St
consBufferAsChild b = searchBuffers . _Just . P.focus . setFocusedSubtree
                      %~ consUnderAndFocus (pTreeLeaf b)

cons_focusedViewResult_asChildOfBuffer :: St -> Either String St
cons_focusedViewResult_asChildOfBuffer st =
  prefixLeft "cons_focusedViewResult_asChild" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocusedBuffer
  (ptrv :: PTree RsltView) <-
    let s = "getFocusedSubtree returned Nothing from bufferRsltViewPorest."
    in maybe (Left s) (maybe (Left s) Right) $
       b ^? bufferRsltViewPorest . _Just . P.focus . getFocusedSubtree
  b' <- bufferFromRsltViewTree ptrv
  Right $ st & hideReassurance & consBufferAsChild b'

debugging_cons_focusedViewResult_asChildOfBuffer ::
  St -> Either String (Buffer, PTree RsltView, Buffer)
debugging_cons_focusedViewResult_asChildOfBuffer st =
  prefixLeft "cons_focusedViewResult_asChild" $ do
  b :: Buffer <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocusedBuffer
  (ptrv :: PTree RsltView) <-
    let s = "getFocusedSubtree returned Nothing from bufferRsltViewPorest."
    in maybe (Left s) (maybe (Left s) Right) $
       b ^? bufferRsltViewPorest . _Just . P.focus . getFocusedSubtree
  b' <- bufferFromRsltViewTree ptrv
  Right $ (b, ptrv, b')

moveFocusedBuffer :: Direction -> St -> St
moveFocusedBuffer d = searchBuffers . _Just %~ moveFocusInPorest d
