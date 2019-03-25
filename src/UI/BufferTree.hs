module UI.BufferTree (
    consBufferAtTop      -- ^ Buffer -> St -> Either String St
  , consBufferAsChild    -- ^ Buffer -> St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> Either String St
  ) where

import qualified Data.Vector as V

import Control.Arrow
import Lens.Micro hiding (has)

import UI.ITypes
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

moveFocusedBuffer :: Direction -> St -> Either String St
moveFocusedBuffer d st = prefixLeft "moveFocusedBuffer" $ do
  (vath, bf) <- moveFocusInVorest d (st^.vathToBuffer, st^.buffers)
  Right $ st & vathToBuffer .~ vath
             & buffers .~ bf
