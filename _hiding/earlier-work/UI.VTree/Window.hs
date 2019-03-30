{-# LANGUAGE ScopedTypeVariables #-}

module UI.Window (
  showBufferAndViewPaths -- ^ St -> St
  ) where

import qualified Data.Map                 as M
import           Lens.Micro
import qualified Data.Text.Zipper.Generic as TxZ

import qualified Brick.Widgets.Edit       as B

import UI.ITypes


showBufferAndViewPaths :: St -> St -- ^ for debugging
showBufferAndViewPaths st =
  showReassurance (s ++ "\n" ++ t) st where
  s = "Path to RlstView: " ++ show (st ^. stBuffer st . bufferPath)
  t = "Vath to Buffer: "   ++ show (st ^. vathToBuffer)
