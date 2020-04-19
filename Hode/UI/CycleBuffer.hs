{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.UI.CycleBuffer (
    updateBlockingCycles  -- ^ St ->          Either String St
  , updateCycleBuffer     -- ^ St ->          Either String St
  , bufferFromPath        -- ^ St -> ViewQuery -> (TpltAddr,[Addr])
                          -- -> Either String Buffer
  , insertBuffer_byQuery  -- ^ ViewQuery -> St -> St
  , delete_cycleBuffer    -- ^ St ->          Either String St
  ) where

import           Prelude hiding (pred)

import           Control.Lens
import qualified Data.Set as S
import qualified Data.List.PointedList as P

import Hode.Hash.Lookup
import Hode.PTree
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.UI.ExprTree
import Hode.UI.Util
import Hode.UI.Types.Names
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window
import Hode.Util.Misc


updateBlockingCycles :: St -> Either String St
-- TODO ? This is inefficient -- it recomputes all cycles each time.
updateBlockingCycles st =
  prefixLeft "updateBlockingCycles: " $
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

-- | `updateCycleBuffer st` updates the cycle buffer to reflect
-- what is now in `st ^. blockingCycles`. If there are no blocking cycles,
-- the cycle buffer is removed.
updateCycleBuffer :: St -> Either String St
updateCycleBuffer _st =
  prefixLeft "cycleBuffer: " $ do
  case _st ^. blockingCycles of
    Just (c:_) -> do
      cb :: Buffer <- bufferFromPath _st CycleView c
      _st :: St <- return $
        case _st ^. stGetTopLevelBuffer_byQuery CycleView of
          Nothing -> insertBuffer_byQuery CycleView _st
          _       ->                                _st
      Right ( _st & stSetTopLevelBuffer_byQuery CycleView .~ cb
              & showReassurance "Cycle detected. Add, move, and replace will be disabled until it is broken. See Cycle Buffer." )
    _ -> -- applies both to Just [] and to Nothing
        (showingInMainWindow .~ SubgraphBuffer)
        . (blockingCycles .~ Nothing)
        . showReassurance "No cycles identified."
        <$> delete_cycleBuffer _st

bufferFromPath :: St -> ViewQuery -> (TpltAddr,[Addr])
               -> Either String Buffer
bufferFromPath st vq (t,c) =
  prefixLeft "bufferFromPath: " $ do
  p :: Porest ExprRow <-
    ( -- Focus on the first child (the template),
      -- rather than on the root of the porest.
      P.focus . pTreeHasFocus .~ True )
    <$> addrsToExprRows st mempty (t:c)
  Right $ Buffer
    { _bufferExprRowTree = PTree
      { _pTreeLabel = exprRow_fromQuery vq
      , _pTreeHasFocus = False
      , _pMTrees = Just p } }

-- | Insert an empty cycle buffer before the current focus.
insertBuffer_byQuery :: ViewQuery -> St -> St
insertBuffer_byQuery vq =
  searchBuffers . _Just %~
  insertLeft_noFocusChange
  ( pTreeLeaf $ bufferFrom_viewQuery vq )

-- | PITFALL: Assumes the Cycle Buffer is top-level and unique,
-- and that it has no child buffers.
-- (Any child buffers would be deleted too.)
delete_cycleBuffer :: St -> Either String St
delete_cycleBuffer st =
  prefixLeft "delete_cycleBuffer: " $
  case st ^. searchBuffers of
  Nothing -> Left "searchBuffers is empty."
  Just (_pb :: Porest Buffer) -> do
    let topIsCycleBuffer :: PTree Buffer -> Bool =
          -- If it is in fact the cycle buffer, the top buffer
          -- should be the only buffer in this tree.
          ( /= (Just CycleView) ) .
          ( ^? pTreeLabel . getBuffer_viewForkType . _VFQuery )
    _pb :: Porest Buffer <- let
      err = Left "No cycle buffer found."
      in maybe err Right $ filterPList topIsCycleBuffer _pb
    _pb :: Porest Buffer <- return $
      -- If the focused node is gone after filtering
      -- (e.g. because it was a child of the cycle buffer),
      -- move focus (arbitrarily) to the top of the buffer porest.
      case _pb ^. P.focus . getFocusedSubtree of
        Nothing -> _pb & P.focus . pTreeHasFocus .~ True
        _ -> _pb
    Right $ st & searchBuffers .~ Just _pb
