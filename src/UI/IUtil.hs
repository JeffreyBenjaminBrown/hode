{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    unEitherSt             -- ^ Either String St -> St -> St

  , emptySt                -- ^ Rslt -> St
  , emptyBuffer            -- ^                                 Buffer
  , emptyPuffer            -- ^                                 Puffer
  , pufferFromRsltViewTree -- ^ PTree RsltView -> Either String Puffer
  , bufferFromRsltViewTree -- ^ VTree RsltView -> Either String Buffer
  ) where

import qualified Data.Map                 as M
import           Lens.Micro

import qualified Brick.Focus              as B
import qualified Brick.Widgets.Edit       as B

import Rslt.RTypes
import UI.ITypes
import UI.Window
import Util.PTree
import Util.VTree


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old & showError s
unEitherSt _ (Right new) = new & showingErrorWindow .~ False

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _buffers = vorestLeaf emptyBuffer
  , _puffers = porestLeaf emptyPuffer
  , _vathToBuffer = (0,[])
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
                     , _bufferView = vTreeLeaf $ VQuery ""
                     , _bufferPath = [] }

emptyPuffer :: Puffer
emptyPuffer = Puffer { _pufferQuery = "(empty puffer)"
                     , _pufferView = pTreeLeaf $ VQuery "" }

-- | TODO : This ought to handle `VMember`s and `VCenterRole`s too.
pufferFromRsltViewTree :: PTree RsltView -> Either String Puffer
pufferFromRsltViewTree vt = do
  let (rsltView :: RsltView) = _pTreeLabel vt
  viewResult <- case rsltView of
    VResult x -> Right x
    _ -> Left $ "bufferFromRsltViewTree called from a non-VResult."
  Right $ Puffer { _pufferQuery = viewResult ^. viewResultString
                 , _pufferView = vt }

bufferFromRsltViewTree :: VTree RsltView -> Either String Buffer
bufferFromRsltViewTree vt = do
  let (rsltView :: RsltView) = _vTreeLabel vt
  viewResult <- case rsltView of
    VResult x -> Right x
    _ -> Left $ "bufferFromRsltViewTree called from a non-VResult."
  Right $ Buffer { _bufferQuery = viewResult ^. viewResultString
                 , _bufferView = vt
                 , _bufferPath = [] }
