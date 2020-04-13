{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.ExprTree.Sort (
  sortFocusAndPeers -- ^ (BinOrientation, TpltAddr) -> St -> Either String St
  , addSelections_toSortedRegion -- ^                  St -> Either String St
  ) where

import           Control.Lens hiding (re)
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


-- | `sortFocusAndPeers (bo, t) st` finds the focused expr `e`
-- in the focused buffer of `st`, and its `peers` in the view.
-- and sorts them all according to `(bo,t)`.
-- It also sets the childSort field of their parents to `Just (bo,t)`
sortFocusAndPeers ::
  (BinOrientation, TpltAddr) -> St -> Either String St
sortFocusAndPeers (bo, t) st =
  prefixLeft "sortFocusAndPeers: " $ do
  peers :: Porest ExprRow <-
    case st ^? stFocusPeers of Just x  -> Right x
                               Nothing -> Left $ "Focused expr has no peers -- probably because it's the root of the view."
  let r :: Rslt = st ^. appRslt
      mas :: [Maybe Addr] =
        map (^? pTreeLabel . exprRow_addr) $ toList peers
  as :: [Addr] <-
    let f :: Maybe Addr -> Either String Addr
        f Nothing = Left $ "Sort failed. Probably because the focused node is a view-gropuiing node, as opposed to an expression in the graph. Try moving the cursor and re-executing that command."
        f (Just a) = Right a
    in mapM f mas

  (sorted :: [Addr], isol :: [Addr]) <-
    kahnSort r (bo,t) as
  let sortedSet :: Set Addr = S.fromList sorted
      order :: [Addr]       = sorted ++ isol
      peers1 :: Porest ExprRow = -- sort
        sortPList_asList
        ( maybe (error "impossible") id
          . (^? pTreeLabel . exprRow_addr))
        order peers
  peers2 :: Porest ExprRow <- let -- modify _boolProps
    f :: PTree ExprRow -> Either String (PTree ExprRow)
    f er = do
      a :: Addr <- maybe (Left "peers2: something has no Addr.") Right
        $ er ^? pTreeLabel . viewExprNode . _VenExpr . viewExpr_Addr
      Right $ er & ( pTreeLabel . boolProps . inSortGroup
                     .~ elem a sortedSet )
    in mapM f peers1

  Right $ st &
    ( stSet_focusedBuffer . bufferExprRowTree . setParentOfFocusedSubtree
      . pMTrees . _Just                     .~ peers2 ) &
    ( stSet_focusedBuffer . bufferExprRowTree . setParentOfFocusedSubtree
      . pTreeLabel . otherProps . childSort .~ Just (bo,t) )

-- ^ `addSelections_toSortedRegion st`
-- adds all the selected peers of the currently focused node
-- to the set of its peers that are sorted. It adds relationships
-- to the graph, such that the new nodes end up at the bottom of the sorted
-- region, in the same order (relative to each other) as they had before.
-- It also reorders the appearance of those expressions in the buffer.
--
-- PITFALL: Only tested by hand. Here's how:
--   r = nInserts (mkRslt mempty) ["a # b","c","d","e","f","(/t /_ x /_) #is transitive"]
--   st <- uiFromRslt r
--   Then run these commands
--     /f a | b | c | d | e | f
--     /sl (/t /_ x /_)
--   Then select (`M-X`) the expressions d and e.
--   Then include them in the sort (`M-i`).
--   The display should now read "a,b,d,e,c,f", and a-d should be
--      colored differently from c and f in the how-it's-sorted column.
addSelections_toSortedRegion :: St -> Either String St
addSelections_toSortedRegion _st =
  prefixLeft "addSelections_toSortedRegion: " $ do

  -- fetch stuff
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
  -- `unseld` : rows not in the sort, to stay out of it
  -- `seld` : rows to be added to the sort
  -- `inSort` : rows already in the sort
  -- (`outSort` is only used to create `unseld` and `seld`.)
  let inSort, outSort, _seld, unseld :: [PTree ExprRow]
      (inSort, outSort) =
        L.partition (^. pTreeLabel . boolProps . inSortGroup) peerErs
      (_seld,   unseld)  =
        L.partition (^. pTreeLabel . boolProps . selected) outSort

  if null _seld then Left "Nothing is selected here."
    else Right ()
  _seld <- Right $ _seld &
    traversed . pTreeLabel . boolProps . inSortGroup .~ True

  -- add new relationships to the `Rslt`
  let inSortAs :: [Addr] = inSort ^.. traversed . pTreeLabel . exprRow_addr
      seldAs   :: [Addr] = _seld  ^.. traversed . pTreeLabel . exprRow_addr
  _r :: Rslt <- insertChain (bo,t) seldAs _r
  _r :: Rslt <- case lastOf traversed inSortAs of
    Nothing -> Right _r
    Just (a :: Addr) ->
      -- connect (last of old sorted) to (first of new sorted)
      let re :: RefExpr = case bo of
            LeftEarlier  -> Rel' $ Rel [a,head seldAs] t
            RightEarlier -> Rel' $ Rel [head seldAs,a] t
      in insert re _r

  Right $ _st
    & appRslt .~ _r
    & ( -- reorder the `ExprRow`s
        stSet_focusedBuffer . bufferExprRowTree
        . setPeersOfFocusedSubtree . _Just
        .~ ( maybe (error "impossible") id
             $ P.fromList $ inSort ++ _seld ++ unseld ) )
    & showReassurance "Selections have been added to the order that currently orders the focused expression and its peers."
