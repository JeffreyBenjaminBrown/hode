{-# LANGUAGE ScopedTypeVariables #-}

module UI.BufferTree (
    consPufferAtTop      -- ^ Buffer -> St -> St
  , consBufferAtTop      -- ^ Buffer -> St -> St
  , consPufferAsChild    -- ^ Buffer -> St -> St
  , consBufferAsChild    -- ^ Buffer -> St -> Either String St
  , cons_focusedViewResult_asChild_inPuffer -- ^ St -> Either String St
  , cons_focusedViewResult_asChild -- ^ St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> Either String St
  , moveFocusedPuffer -- ^ Direction -> St -> St
  ) where

import qualified Data.Vector as V

import           Control.Arrow
import qualified Data.List.PointedList as P
import           Lens.Micro hiding (has)

import UI.ITypes
import UI.IUtil
import UI.Window
import Util.Direction
import Util.Misc
import Util.PTree
import Util.VTree


consPufferAtTop :: Puffer -> St -> St
consPufferAtTop b st = st & puffers . setList %~ (pTreeLeaf b :)

consBufferAtTop :: Buffer -> St -> St
consBufferAtTop b st =
  st & buffers %~ V.cons (vTreeLeaf b)
     & vathToBuffer .~ (0,[])

consPufferAsChild :: Puffer -> St -> St
consPufferAsChild b st = let
  newFocus :: PTree Puffer
  newFocus = pTreeLeaf b & pTreeHasFocus .~ True
  consNewFocus    :: St -> St
  consNewFocus    = puffers . P.focus . setFocusedSubtree .
                    pMTrees . _Just .
                    setList %~ (newFocus :)
  unFocusOldFocus :: St -> St
  unFocusOldFocus = puffers . P.focus . setFocusedSubtree .
                    pTreeHasFocus .~ False
  -- PITFALL: Order of these ops matters. The old focus should only be
  -- unfocused after the new focus is inserted; otherwise the place to
  -- insert cannot be found. (& is left-infix.)
  in st & consNewFocus & unFocusOldFocus

consBufferAsChild :: Buffer -> St -> Either String St
consBufferAsChild b st = prefixLeft "consEmptyBuffer" $ do
  _ <- vathInBounds (st^.buffers) (st^.vathToBuffer)
  let vath = st^.vathToBuffer
  Right $ st & buffers . atVath vath . vTrees %~ V.cons (vTreeLeaf b)
             & vathToBuffer %~ second (++ [0])

cons_focusedViewResult_asChild_inPuffer :: St -> Either String St
cons_focusedViewResult_asChild_inPuffer st =
  prefixLeft "cons_focusedViewResult_asChild" $ do
  p <- let s = "stPuffer returned Nothing."
    in maybe (Left s) Right $ st ^. stGetFocusedPuffer
  (pt :: PTree RsltView) <-
    let s = "getFocusedSubtree returned Nothing from pufferView."
    in maybe (Left s) Right $ (p^.pufferView) ^. getFocusedSubtree
  p' <- pufferFromRsltViewTree pt
  Right $ st & hideReassurance & consPufferAsChild p'

cons_focusedViewResult_asChild :: St -> Either String St
cons_focusedViewResult_asChild st =
  prefixLeft "cons_focusedViewResult_asChild" $ do
  b <- let s = "stBuffer returned Nothing."
    in maybe (Left s) Right $ st              ^? stBuffer st
  (vt :: VTree RsltView) <- let s = "atPath returned Nothing."
    in maybe (Left s) Right $ (b^.bufferView) ^? atPath (b^.bufferPath)
  b' <- bufferFromRsltViewTree vt
  st & hideReassurance & consBufferAsChild b'

moveFocusedBuffer :: Direction -> St -> Either String St
moveFocusedBuffer d st = prefixLeft "moveFocusedBuffer" $ do
  (vath, bf) <- moveFocusInVorest d (st^.vathToBuffer, st^.buffers)
  Right $ st & vathToBuffer .~ vath
             & buffers .~ bf

moveFocusedPuffer :: Direction -> St -> St
moveFocusedPuffer d st = st & puffers . P.focus %~ moveFocusInPTree d
