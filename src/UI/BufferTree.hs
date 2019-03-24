module UI.BufferTree where

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
