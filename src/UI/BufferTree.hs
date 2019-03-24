module UI.BufferTree (
    consEmptyBuffer   -- ^              St -> Either String St
  , moveFocusedBuffer -- ^ Direction -> St -> Either String St
  ) where

import qualified Data.Vector as V

import           Lens.Micro hiding (has)

import UI.ITypes
import UI.IUtil
import Util.Misc
import Util.VTree


consEmptyBuffer :: St -> Either String St
consEmptyBuffer st = prefixLeft "consEmptyBuffer" $ do
  let v = st ^. vathToBuffer
      theCons = V.cons $ vTreeLeaf emptyBuffer
  case v of
    (_,[]) -> Right $ st & buffers %~ theCons
                         & vathToBuffer .~ (0,[])
    (i,p) -> Right $ st & buffers . atVath (i,p) . vTrees %~ theCons
                        & vathToBuffer .~ (i, replaceLast' 0 p)

moveFocusedBuffer :: Direction -> St -> Either String St
moveFocusedBuffer d st = prefixLeft "moveFocusedBuffer" $ do
  (vath, bf) <- moveFocusInVorest d (st^.vathToBuffer, st^.buffers)
  Right $ st & vathToBuffer .~ vath
             & buffers .~ bf
