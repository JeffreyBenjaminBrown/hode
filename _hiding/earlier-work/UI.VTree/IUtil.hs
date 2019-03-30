{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    emptyBuffer            -- ^                                 Buffer
  , bufferFromRsltViewTree -- ^ VTree RsltView -> Either String Buffer
  ) where

import qualified Data.List.PointedList as P
import qualified Data.Map              as M
import           Lens.Micro

import qualified Brick.Focus           as B
import qualified Brick.Widgets.Edit    as B

import Rslt.RTypes
import UI.ITypes
import UI.Window
import Util.PTree
import Util.VTree


emptyBuffer :: Buffer
emptyBuffer = Buffer { _bufferQuery = "(empty buffer)"
                     , _bufferView = vTreeLeaf $ VQuery ""
                     , _bufferPath = [] }

bufferFromRsltViewTree :: VTree RsltView -> Either String Buffer
bufferFromRsltViewTree vt = do
  let (rsltView :: RsltView) = _vTreeLabel vt
  viewResult <- case rsltView of
    VResult x -> Right x
    _ -> Left $ "bufferFromRsltViewTree called from a non-VResult."
  Right $ Buffer { _bufferQuery = viewResult ^. viewResultString
                 , _bufferView = vt
                 , _bufferPath = [] }
