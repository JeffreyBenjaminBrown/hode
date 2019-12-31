{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    emptySearchBuffer            -- ^                                 Buffer
  , buffer_from_exprRowTree -- ^ VTree ViewExprNode -> Either String Buffer
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


emptySearchBuffer :: Buffer
emptySearchBuffer = Buffer { _bufferQuery = "(empty buffer)"
                     , _bufferView = vTreeLeaf $ VQuery ""
                     , _bufferPath = [] }

buffer_from_exprRowTree :: VTree ViewExprNode -> Either String Buffer
buffer_from_exprRowTree vt = do
  let (viewExprNode :: ViewExprNode) = _vTreeLabel vt
  viewResult <- case viewExprNode of
    VenExpr x -> Right x
    _ -> Left $ "buffer_from_exprRowTree called from a non-VenExpr."
  Right $ Buffer { _bufferQuery = viewResult ^. viewExpr_String
                 , _bufferView = vt
                 , _bufferPath = [] }
