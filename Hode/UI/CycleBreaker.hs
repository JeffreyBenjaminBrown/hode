{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.UI.CycleBreaker where

import           Control.Lens
import qualified Data.List.PointedList as P

import Hode.PTree.Initial
import Hode.Rslt.RTypes
import Hode.UI.BufferRowTree
import Hode.UI.IUtil
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc


setCycleBuffer :: St -> Either String St
setCycleBuffer st =
  prefixLeft "cycleBuffer:" $ do
  cs :: [Cycle] <-
    case st ^? stGetFocused_Buffer . _Just . bufferCycles
    of Nothing -> Left "Focused buffer not found."
       Just cs -> Right cs
  case cs of
    (c:_) -> do
      p :: Porest BufferRow <-
        (P.focus . pTreeHasFocus .~ True)
        <$> addrsToBufferRows st mempty c
      Right ( st & cycleBreaker .~ p
              & blockedByCycles .~ True
              & showingInMainWindow .~ CycleBreaker
              & showReassurance "Please break this cycle." )
    [] -> Right ( st & cycleBreaker .~ emptyCycleBreaker
                  & blockedByCycles .~ False
                  & showingInMainWindow .~ Results
                  & showReassurance "Cycles eliminated." )
