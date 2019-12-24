{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.UI.CycleBreaker where

import           Control.Lens
import qualified Data.Set as S
import qualified Data.List.PointedList as P

import Hode.Hash.HLookup
import Hode.PTree.Initial
import Hode.Rslt.Binary
import Hode.Rslt.RTypes
import Hode.UI.BufferRowTree
import Hode.UI.IUtil
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc


-- | If the focused buffer has cycles recorded in `bufferCycles`,
-- this recomputes them.
updateFocusedBufferCycles :: St -> Either String St
-- TODO ? This is inefficient -- it recomputes all cycles each time.
updateFocusedBufferCycles st = do
  cs0 :: [Cycle] <-
    case st ^? stGet_focusedBuffer . _Just . bufferCycles
    of Nothing -> Left "Focused buffer not found."
       Just cs -> Right cs

  if null cs0 then Right st
    else do
    let uniqueKernels :: [(TpltAddr,Addr)] =
          S.toList . S.fromList -- discard duplicates
          $ map (_2 %~ head) cs0
        ci (tplt, start) = cyclesInvolving
          (st ^. appRslt) SearchLeftward tplt start
    cs1 :: [Cycle] <-
      concat <$> mapM ci uniqueKernels
    Right $ st &
      stSet_focusedBuffer . bufferCycles .~ cs1

-- | Updates the cycle buffer to reflect what is now in
-- the focused buffer's `bufferCycles` field.
updateCycleBuffer :: St -> Either String St
updateCycleBuffer st =
  prefixLeft "cycleBuffer:" $ do
  cs :: [Cycle] <-
    case st ^? stGet_focusedBuffer . _Just . bufferCycles
    of Nothing -> Left "Focused buffer not found."
       Just cs -> Right cs
  case cs of
    ((t,c):_) -> do
      p :: Porest BufferRow <-
        (P.focus . pTreeHasFocus .~ True)
        <$> addrsToBufferRows st mempty (t:c)
      Right ( st & cycleBreaker .~ p
              & blockedByCycles .~ True
              & showingInMainWindow .~ CycleBreaker
              & showReassurance "Please break this cycle." )
    [] -> Right ( st & cycleBreaker .~ emptyCycleBreaker
                  & blockedByCycles .~ False
                  & showingInMainWindow .~ Results
                  & showReassurance "Cycles eliminated." )
