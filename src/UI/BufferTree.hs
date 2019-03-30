{-# LANGUAGE ScopedTypeVariables #-}

module UI.BufferTree (
    consBufferAtTop      -- ^ Buffer -> St -> St
  , consBufferAsChild    -- ^ Buffer -> St -> St
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


consBufferAtTop :: Buffer -> St -> St
consBufferAtTop b st = st & buffers . setList %~ (pTreeLeaf b :)

consBufferAsChild :: Buffer -> St -> St
consBufferAsChild b st = let
  newFocus :: PTree Buffer
  newFocus = pTreeLeaf b & pTreeHasFocus .~ True
  consNewFocus    :: St -> St
  consNewFocus    = buffers . P.focus . setFocusedSubtree .
                    pMTrees . _Just .
                    setList %~ (newFocus :)
  unFocusOldFocus :: St -> St
  unFocusOldFocus = buffers . P.focus . setFocusedSubtree .
                    pTreeHasFocus .~ False
  -- PITFALL: Order of these ops matters. The old focus should only be
  -- unfocused after the new focus is inserted; otherwise the place to
  -- insert cannot be found. (& is left-infix.)
  in st & consNewFocus & unFocusOldFocus

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
moveFocusedBuffer d st = st & buffers . P.focus %~ moveFocusInPTree d
