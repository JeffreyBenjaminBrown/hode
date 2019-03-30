{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    unEitherSt             -- ^ Either String St -> St -> St

  , emptySt                -- ^ Rslt -> St
  , emptyBuffer            -- ^                                 Buffer
  , bufferFromRsltViewTree -- ^ PTree RsltView -> Either String Buffer
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


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old & showError s
unEitherSt _ (Right new) = new & showingErrorWindow .~ False

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _buffers = porestLeaf emptyBuffer
               & P.focus . pTreeHasFocus .~ True
  , _uiError   = ""
  , _reassurance = "It's all good."
  , _commands  = B.editor (BrickOptionalName Commands) Nothing ""
  , _commandHistory = []
  , _appRslt   = r
  , _showingErrorWindow = False
  , _showingInMainWindow = Results
  , _showingOptionalWindows = M.fromList [ (Commands   , True)
                                         , (Reassurance, True) ]
  }

emptyBuffer :: Buffer
emptyBuffer = Buffer { _bufferQuery = "(empty buffer)"
                     , _bufferRsltViewTree = pTreeLeaf $ VQuery "" }

-- | TODO : This ought to handle `VMember`s and `VCenterRole`s too.
bufferFromRsltViewTree :: PTree RsltView -> Either String Buffer
bufferFromRsltViewTree vt = do
  let (rsltView :: RsltView) = _pTreeLabel vt
  viewResult <- case rsltView of
    VResult x -> Right x
    _ -> Left $ "bufferFromRsltViewTree called from a non-VResult."
  Right $ Buffer { _bufferQuery = viewResult ^. viewResultString
                 , _bufferRsltViewTree = vt }
