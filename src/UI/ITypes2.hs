{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.ITypes2 where

import           Data.Functor.Foldable.TH
import           Lens.Micro.TH
import           Data.Vector (Vector)

import qualified Brick.Widgets.Edit as B
import qualified Brick.Focus as B

import Rslt.RTypes
import UI.ITypes


data St2 = St2 {
    _st2_focusRing            :: B.FocusRing WindowName
  , _st2_view                 :: View
  , _st2_focusedSubview       :: [Int]
  , _st2_uiError              :: String
  , _st2_commands             :: B.Editor String WindowName
  , _st2_appRslt              :: Rslt
  , _st2_shownInResultsWindow :: ShownInResultsWindow
  }

data View = View {
    _viewFocus :: Int -- ^ maybe out of bounds; that's ok
  , _viewContent :: Either ViewQuery ViewResult
  , _viewSubviews :: Vector View -- ^ PITFALL: permits invalid state.
  -- A `ViewResult`'s children should be `ViewQuery`s, and vice-versa.
  }

type ViewQuery = String

data ViewResult = ViewResult {
    _viewResultAddr :: Addr
  , _viewResultExpr :: Expr
  , _viewResultString :: String }

makeLenses ''St2
makeLenses ''View
makeLenses ''ViewResult

makeBaseFunctor ''View
makeLenses ''ViewF
