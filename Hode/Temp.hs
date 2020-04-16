{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Temp where

--import qualified Data.List             as L
--import qualified Data.List.PointedList as P
--import qualified Data.Set              as S
--import           Control.Lens
--import           Data.Set (Set)
--import qualified Data.Set as S
--import Hode.Hode


import           Control.Lens hiding (re, below)
import           Data.Foldable (toList)
import qualified Data.List             as L
import qualified Data.List.PointedList as P
import           Data.Set (Set)
import qualified Data.Set              as S

import Hode.Hash.Lookup
import Hode.PTree
import Hode.Rslt.Binary
import Hode.Rslt.Edit
import Hode.Rslt.Sort
import Hode.Rslt.Types
import Hode.UI.CycleBuffer
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.Window (showReassurance)
import Hode.Util.Misc

import Hode.UI.ExprTree.Sort


raiseSelection_inSortedRegion :: St -> Either String St
raiseSelection_inSortedRegion _st =
  prefixLeft "removeSelections_fromSortedRegion: " $ do

  -- fetch stuff
  _peers :: Porest ExprRow <-
    case _st ^? stFocusPeers of Just x  -> Right x
                                Nothing -> Left $ "Focused expr has no peers -- probably because it's the root of the view."
  case _peers ^. P.focus . pTreeLabel . viewExprNode of
    VenExpr _ -> Right ()
    _ -> Left $ "Focused node is not an Expr in the graph. (Instead it's probably a grouping node.)"
  (bo :: BinOrientation, t :: TpltAddr) <-
    let errMsg = "Focused node and its peers have not been sorted."
    in maybe (Left errMsg) Right
       $ _st ^? stFocusGroupOrder
  let peerErs :: [PTree ExprRow] = toList _peers
      _r :: Rslt = _st ^. appRslt

  -- Partition the list into:
  -- `unseld` : rows in the sort, to stay there
  -- `seld` : rows to be removed from the sort
  -- `outSort` : rows already not part of the sort
  -- (`inSort` is only used to create `unseld` and `seld`.)
  let inSort, outSort :: [PTree ExprRow]
      (inSort, outSort) =
        L.partition (^. pTreeLabel . boolProps . inSortGroup) peerErs
  ( _above :: [PTree ExprRow],
    _seld :: [PTree ExprRow],
    below :: [PTree ExprRow]) <-
    beforeDuringAfter (^. pTreeLabel . boolProps .selected ) inSort
  case null _above of
    True -> Right _st -- can't get any higher
    False -> do
      if null _seld then Left "Nothing is selected here."
        else Right ()

      ( _above :: [PTree ExprRow], -- extract last elt "crosser" from _above
        crosser :: PTree ExprRow) <- -- will move to the other side of _seld
        (_2 %~ head) <$> -- safe because _above is not null
        Right (splitAt (length _above - 1) _above)

      let crosserA :: Addr =
            maybe (error "impossible: we checked that the focused node is an Expr, so the crosser should be one too.") id $
            crosser                  ^?              pTreeLabel . exprRow_addr
          seldAs  :: [Addr] = _seld  ^.. traversed . pTreeLabel . exprRow_addr
      _r <- separateSimply t [crosserA] seldAs _r
      error "TODO : Test if they are still connected. If so, throw to ProblematicChain view. If not, connect `crosser` to the other side, and reorder the ExprRows."
