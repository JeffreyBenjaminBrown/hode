{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    emptyBuffer            -- ^                                 Buffer
  , buffer_from_bufferRowTree -- ^ VTree RsltView -> Either String Buffer
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

buffer_from_bufferRowTree :: VTree RsltView -> Either String Buffer
buffer_from_bufferRowTree vt = do
  let (rsltView :: RsltView) = _vTreeLabel vt
  viewResult <- case rsltView of
    VExpr x -> Right x
    _ -> Left $ "buffer_from_bufferRowTree called from a non-VExpr."
  Right $ Buffer { _bufferQuery = viewResult ^. viewResultString
                 , _bufferView = vt
                 , _bufferPath = [] }
