{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.UI.CycleBreaker where

import           Control.Lens
import qualified Data.List.PointedList as P

import Hode.Rslt.RTypes
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.BufferRowTree
import Hode.UI.Window
import Hode.Util.Misc
import Hode.PTree.Initial


setCycleBuffer :: St -> Either String St
setCycleBuffer st =
  prefixLeft "cycleBuffer:" $ do
  as :: [Addr] <-
    case st ^? stGetFocused_Buffer . _Just . bufferCycles
    of Nothing -> Left "Focused buffer not found."
       Just [] -> Left "Focused buffer has no cycles."
       Just (as:_) -> Right as
  p :: Porest BufferRow <-
    (P.focus . pTreeHasFocus .~ True)
    <$> addrsToBufferRows st mempty as
  Right ( st & cycleBreaker .~ p
          & showReassurance "Cycle buffer reset." )
