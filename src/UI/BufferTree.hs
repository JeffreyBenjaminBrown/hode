module UI.BufferTree (
    consEmptyTopBuffer   -- ^           St -> St
  , consEmptyChildBuffer -- ^           St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> Either String St
  ) where

import qualified Data.Vector as V

import Control.Arrow
import Lens.Micro hiding (has)

import UI.ITypes
import UI.IUtil
import Util.Misc
import Util.VTree


consEmptyTopBuffer :: St -> St
consEmptyTopBuffer st =
  st & buffers %~ V.cons (vTreeLeaf emptyBuffer)
     & vathToBuffer .~ (0,[])

consEmptyChildBuffer :: St -> Either String St
consEmptyChildBuffer st = prefixLeft "consEmptyBuffer" $ do
  _ <- vathInBounds (st^.buffers) (st^.vathToBuffer)
  let theCons = V.cons $ vTreeLeaf emptyBuffer
      (i,p) = st^.vathToBuffer
  Right $ st & buffers . atVath (i,p) . vTrees %~ theCons
             & vathToBuffer %~ second (++ [0])

moveFocusedBuffer :: Direction -> St -> Either String St
moveFocusedBuffer d st = prefixLeft "moveFocusedBuffer" $ do
  (vath, bf) <- moveFocusInVorest d (st^.vathToBuffer, st^.buffers)
  Right $ st & vathToBuffer .~ vath
             & buffers .~ bf
