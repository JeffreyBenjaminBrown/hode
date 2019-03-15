module UI.IUtil where

import qualified Data.Vector as V
import Lens.Micro

import UI.ITypes


viewLeaf :: View -> ViewTree
viewLeaf v = ViewTree {
    _viewFocus = 0
  , _viewIsFocused = False
  , _viewContent = v
  , _viewSubviews = V.empty }
