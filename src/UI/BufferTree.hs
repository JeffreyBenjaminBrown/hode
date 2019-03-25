{-# LANGUAGE ScopedTypeVariables #-}

module UI.BufferTree (
    consBufferAtTop      -- ^ Buffer -> St -> Either String St
  , consBufferAsChild    -- ^ Buffer -> St -> Either String St
  , cons_focusedViewResult_asChild -- ^ St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> Either String St
  ) where

import qualified Data.Vector as V

import Control.Arrow
import Lens.Micro hiding (has)

import UI.ITypes
import UI.IUtil
import UI.Window
import Util.Misc
import Util.VTree


consBufferAtTop :: Buffer -> St -> St
consBufferAtTop b st =
  st & buffers %~ V.cons (vTreeLeaf b)
     & vathToBuffer .~ (0,[])

consBufferAsChild :: Buffer -> St -> Either String St
consBufferAsChild b st = prefixLeft "consEmptyBuffer" $ do
  _ <- vathInBounds (st^.buffers) (st^.vathToBuffer)
  let vath = st^.vathToBuffer
  Right $ st & buffers . atVath vath . vTrees %~ V.cons (vTreeLeaf b)
             & vathToBuffer %~ second (++ [0])

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
