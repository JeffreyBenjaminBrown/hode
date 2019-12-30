{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.UI.CycleBuffer (
    updateBlockingCycles  -- ^ St ->          Either String St
  , updateCycleBuffer     -- ^ St ->          Either String St
  , cycleBuffer_fromAddrs -- ^ St -> Cycle -> Either String Buffer
  , insert_cycleBuffer    -- ^ St ->                        St
  , delete_cycleBuffer    -- ^ St ->          Either String St
  ) where

import           Prelude hiding (pred)

import           Control.Lens
import qualified Data.Set as S
import qualified Data.List.PointedList as P

import Hode.Hash.HLookup
import Hode.PTree.Initial
import Hode.Rslt.Binary
import Hode.Rslt.RTypes
import Hode.UI.ExprTree
import Hode.UI.IUtil
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Types.Views2
import Hode.UI.Window
import Hode.Util.Misc


-- | If the focused buffer has cycles recorded in `bufferCycles`,
-- this recomputes them.
updateBlockingCycles :: St -> Either String St
-- TODO ? This is inefficient -- it recomputes all cycles each time.
updateBlockingCycles st =
  case st ^. blockingCycles of
  Nothing -> Right st
  Just cs0 -> do
    let uniqueKernels :: [(TpltAddr,Addr)] =
          S.toList . S.fromList -- discard duplicates
          $ map (_2 %~ head) cs0
        ci (tplt, start) = cyclesInvolving
          (st ^. appRslt) SearchLeftward tplt start
    cs1 :: [Cycle] <-
      concat <$> mapM ci uniqueKernels
    Right $ st & blockingCycles .~ Just cs1

-- | Updates the cycle buffer to reflect what is now in
-- the focused buffer's `bufferCycles` field.
updateCycleBuffer :: St -> Either String St
updateCycleBuffer st0 =
  prefixLeft "cycleBuffer:" $ do
  case st0 ^. blockingCycles of
    Just (c:_) -> do
      cb <- cycleBuffer_fromAddrs st0 c
      let st1 = case st0 ^. stGet_cycleBuffer of
                  Nothing -> insert_cycleBuffer st0
                  _ -> st0
      Right ( st1 & stSet_cycleBuffer .~ cb
              & showReassurance "Cycle detected. Add, move and replace are disabled. See Cycle Buffer." )
    _ -> -- applies both to Just [] and to Nothing
        (showingInMainWindow .~ SearchBuffer)
        . (blockingCycles .~ Nothing)
        . showReassurance "No cycles identified."
        <$> delete_cycleBuffer st0

cycleBuffer_fromAddrs :: St -> Cycle -> Either String Buffer
cycleBuffer_fromAddrs st (t,c) = do
  p :: Porest ExprRow <-
    (P.focus . pTreeHasFocus .~ True)
    <$> addrsToExprRows st mempty (t:c)
  Right $ Buffer
    { _bufferExprRowTree = PTree
      { _pTreeLabel = exprRow_from_viewExprNode $
        VFork' $ ViewFork'
          { _viewForkCenter' = Nothing
          , _viewForkSortTplt' = Nothing
          , _viewForkType' = VFQuery' CycleView }
      , _pTreeHasFocus = False
      , _pMTrees = Just p } }

-- | Inserts an empty cycle buffer before the current focus.
insert_cycleBuffer :: St -> St
insert_cycleBuffer =
  searchBuffers . _Just %~
  insertLeft_noFocusChange (pTreeLeaf emptyCycleBuffer)

-- | PITFALL: Assumes the Cycle Buffer is top-level and unique.
delete_cycleBuffer :: St -> Either String St
delete_cycleBuffer st =
  prefixLeft "delete_cycleBuffer:" $
  case st ^. searchBuffers of
    Nothing -> Left "searchBuffers is empty. (Not a user error.)"
    Just (pb0 :: Porest Buffer) -> do
      let pred :: PTree Buffer -> Bool =
            ( /= (Just CycleView) ) .
            ( ^? pTreeLabel . bufferExprRowTree .
              pTreeLabel . viewExprNode .
              _VFork' . viewForkType' . _VFQuery' )
      pb1 :: Porest Buffer <- let
        err = Left "nothing left after filtering. (Not a user error.)"
        in maybe err Right $ filterPList pred pb0
      let pb2 :: Porest Buffer =
            -- If the focused node is gone after filtering 
            -- (e.g. because it was a child of the cycle buffer),
            -- move focus (arbitrarily) to the top of the buffer porest.
            case pb1 ^. P.focus . getFocusedSubtree of
              Nothing -> pb1 & P.focus . pTreeHasFocus .~ True
              _ -> pb1
      Right $ st & searchBuffers .~ Just pb2
