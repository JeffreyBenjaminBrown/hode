{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Temp where

--import qualified Data.List.PointedList as P
--import           Control.Lens
import qualified Data.Map as M
--import           Data.Set (Set)
--import qualified Data.Set as S
--import Hode.Hode

import           Control.Lens hiding (re)
import           Control.Monad
import           Data.Foldable (toList)
import qualified Data.List             as L
import qualified Data.List.PointedList as P
import           Data.Set (Set)
import qualified Data.Set              as S

import Hode.PTree
import Hode.Rslt.Binary
import Hode.Rslt.Edit
import Hode.Rslt.Sort
import Hode.Rslt.Types
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window (showReassurance)
import Hode.Util.Misc

import Hode.UI.ExprTree.Sort
import Hode.Hash.Types
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
  (bo,t) :: (BinOrientation, TpltAddr) <-
    let errMsg = "Focused node and its peers have not been sorted."
    in maybe (Left errMsg) Right
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

  -- separate `seldAs` and `unseldAs` via `separateSimply ::
  --   TpltAddr -> [Addr] -> [Addr] -> Rslt -> Either String Rslt`
  -- Test(should be a separate function) whether they are still connected.
  -- If they are, then abort this (don't change the Rslt),
  --   and (this should be a separate function) do something like
  --     updateCycleBuffer :: St -> Either String St
  --   to make a `OffscreenConnectionView`.
  -- If they're not, then finish as in `addSelections_toSortedRegion`,
  --   changing hhe Rslt,
  --   reordering the ExprRows, and
  --   giving appropriate reassurance.

  error ""
