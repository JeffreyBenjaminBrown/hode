{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Temp where

--import qualified Data.List.PointedList as P
--import           Control.Lens
--import           Data.Set (Set)
--import qualified Data.Set as S
--import Hode.Hode

import           Control.Lens hiding (re)
import           Data.Foldable (toList)
import qualified Data.List             as L
import qualified Data.List.PointedList as P
import qualified Data.Set              as S

import Hode.PTree
import Hode.Rslt.Binary
import Hode.Rslt.Edit
import Hode.Rslt.Types
import Hode.UI.CycleBuffer
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window (showReassurance)
import Hode.Util.Misc

import Hode.Hash.Lookup


removeSelections_fromSortedRegion :: St -> Either String St
removeSelections_fromSortedRegion _st =
  prefixLeft "removeSelections_fromSortedRegion: " $ do

  -- fetch stuff
  -- TODO this section is identical to one in addSelections_toSortedRegion,
  -- and very similar to one in sortFocusAndPeers
  peers :: Porest ExprRow <-
    case _st ^? stFocusPeers of Just x  -> Right x
                                Nothing -> Left $ "Focused expr has no peers -- probably because it's the root of the view."
  t :: TpltAddr <-
    let errMsg = "Focused node and its peers have not been sorted."
    in maybe (Left errMsg) (Right . snd)
       $ _st ^? stFocusGroupOrder
  let peerErs :: [PTree ExprRow] = toList peers
      _r :: Rslt = _st ^. appRslt

  -- Partition the list into:
  -- `unseld` : rows in the sort, to stay there
  -- `seld` : rows to be removed from the sort
  -- `outSort` : rows already not part of the sort
  -- (`inSort` is only used to create `unseld` and `seld`.)
  let inSort, outSort, _seld, unseld :: [PTree ExprRow]
      (inSort, outSort) =
        L.partition (^. pTreeLabel . boolProps . inSortGroup) peerErs
      (_seld,  unseld)  =
        L.partition (^. pTreeLabel . boolProps . selected) inSort

  if null _seld then Left "Nothing is selected here."
    else Right ()
  _seld <- Right $ _seld &
    traversed . pTreeLabel . boolProps . inSortGroup .~ False

  let unseldAs :: [Addr] = unseld ^.. traversed . pTreeLabel . exprRow_addr
      seldAs   :: [Addr] = _seld  ^.. traversed . pTreeLabel . exprRow_addr

  _r <- separateSimply t unseldAs seldAs _r
  _conns :: [[Addr]] <-
    connections _r SearchLeftward  t seldAs (S.fromList unseldAs)
  _conns :: [[Addr]] <- (_conns ++) <$>
    connections _r SearchRightward t seldAs (S.fromList unseldAs)

    -- TODO : If there are any connections, this is inefficient --
    --   it finds them all, present the user with one of them,
    --   and discards the rest. Once the user breaks one and tries again,
    --   Hode will search for all remaining connections again.
    --   (`removeSelections_fromSortedRegion` repeats some other tasks too,
    --   but they are not likely to be expensive.)

  if null _conns
    then Right $ _st
         & appRslt .~ _r
         & ( -- reorder the `ExprRow`s
             stSet_focusedBuffer . bufferExprRowTree
             . setPeersOfFocusedSubtree . _Just
             .~ ( maybe (error "impossible") id
                  $ P.fromList $ unseld ++ _seld ++ outSort ) )
         & showReassurance "Selections have been removed from the order that currently orders the focused expression and its peers."

    else do
      b :: Buffer <-
        bufferFromPath _st OffscreenConnectionView (t, head _conns)
      _st :: St <- return $
        case _st ^. stGetTopLevelBuffer_byQuery OffscreenConnectionView
        of Nothing -> _st & insertBuffer_byQuery OffscreenConnectionView
           _       -> _st
      Right $ _st
        & stSetTopLevelBuffer_byQuery OffscreenConnectionView .~ b
        & showReassurance "You asked Hode to remove the selected expressions from the ordering that currently orders the focused expression and its peers, but a chain of length > 1 connects them. You can find it in the ProblematicChain buffer. Please break that chain and try again."
