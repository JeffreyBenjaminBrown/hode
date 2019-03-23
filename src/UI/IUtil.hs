{-# LANGUAGE ScopedTypeVariables #-}

module UI.IUtil (
    unEitherSt   -- ^ Either String St -> St -> St

  , emptySt      -- ^ Rslt -> St
  , emptyBuffer  -- ^ Buffer
  , rsltViewLeaf -- ^ RsltView -> VTree RsltView
  ) where

import qualified Data.Map                 as M
import qualified Data.Vector              as V
import           Lens.Micro

import qualified Brick.Focus              as B
import qualified Brick.Widgets.Edit       as B

import Rslt.RLookup
import Rslt.RTypes
import Rslt.Show
import UI.ITypes
import UI.Window
import Util.Misc


unEitherSt :: St -> Either String St -> St
unEitherSt old (Left s) = old & showError s
unEitherSt _ (Right new) = new & showingInMainWindow .~ Results

emptySt :: Rslt -> St
emptySt r = St {
    _focusRing = B.focusRing [BrickOptionalName Commands]
  , _buffers = V.singleton
               $ VTree { _vTreeLabel = emptyBuffer
                       , _vTrees = V.empty
                       , _vTreeFocus = 0
                       , _vTreeIsFocused = False }

  , _vathToBuffer = (0,[])
  , _uiError   = ""
  , _reassurance = "It's all good."
  , _commands  = B.editor (BrickOptionalName Commands) Nothing ""
  , _commandHistory = []
  , _appRslt   = r
  , _showingInMainWindow = Results
  , _showingOptionalWindows = M.fromList [ (Commands   , True)
                                         , (Reassurance, True) ]
  }

emptyBuffer :: Buffer
emptyBuffer = Buffer { _bufferQuery = ""
                     , _bufferView = rsltViewLeaf $ VQuery ""
                     , _bufferPath = [] }

rsltViewLeaf :: RsltView -> VTree RsltView
rsltViewLeaf v = VTree {
    _vTreeLabel = v
  , _vTreeFocus = 0
  , _vTreeIsFocused = False
  , _vTrees = V.empty }
