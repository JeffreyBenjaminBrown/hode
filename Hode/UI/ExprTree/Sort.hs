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
import Hode.Util.Misc


-- | `sortFocusAndPeers (bo, t) st` finds the focused expr `e`
-- in the focused buffer of `st`, and its `peers` in the view.
-- and sorts them all according to `(bo,t)`.
-- It also sets the childSort field of their parents to `Just (bo,t)`
sortFocusAndPeers ::
  (BinOrientation, TpltAddr) -> St -> Either String St
sortFocusAndPeers (bo, t) st =
  prefixLeft "sortFocusAndPeers: " $ do
  peers :: Porest ExprRow <- stFocusPeers st
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

addSelections_toSortedRegion :: St -> Either String St
addSelections_toSortedRegion _st =
  prefixLeft "addSelections_toSortedRegion: " $ do

  -- fetch stuff
  peers :: Porest ExprRow <- stFocusPeers _st
  (bo,t) :: (BinOrientation, TpltAddr) <-
    let errMsg = "Focused node and its peers have not been sorted."
    in maybe (Left errMsg) Right
       $ _st ^? ( stGet_focusedBuffer . bufferExprRowTree
                  . pTreeLabel . otherProps . childSort . _Just )
  let peerErs :: [PTree ExprRow] = toList peers

  -- partition the list into (1) rows already in the sort,
  -- (2) selected things to be added to the sort, and (3) the rest.
  let inSort, outSort, _seld, unseld :: [PTree ExprRow]
      (inSort, outSort) =
        L.partition (^. pTreeLabel . boolProps . inSortGroup) peerErs
      (_seld,   unseld)  =
        L.partition (^. pTreeLabel . boolProps . selected) outSort

  if not $ null _seld then Right ()
    else Left "Nothing has been selected."
  _seld <- Right $
    _seld & traversed . pTreeLabel . boolProps . inSortGroup .~ True

  -- reorder the relevant `ExprRow`s
  _st :: St <- Right $  _st & stSet_focusedBuffer . bufferExprRowTree
    . setPeersOfFocusedSubtree . _Just
    .~ ( maybe (error "impossible") id
         $ P.fromList $ inSort ++ _seld ++ unseld )

  -- add new relationships to the `Rslt`
  let inSortAs :: [Addr] = inSort ^.. traversed . pTreeLabel . exprRow_addr
      seldAs   :: [Addr] = _seld   ^.. traversed . pTreeLabel . exprRow_addr
      _r :: Rslt = _st ^. appRslt
  _r :: Rslt <- insertChain (bo,t) seldAs _r
  _r :: Rslt <- case lastOf traversed inSortAs of
    Nothing -> Right _r
    Just (a :: Addr) ->
      -- connect (last of old sorted) to (first of new sorted)
      let re :: RefExpr = case bo of
            LeftEarlier  -> Rel' $ Rel [a,head seldAs] t
            RightEarlier -> Rel' $ Rel [head seldAs,a] t
      in insert re _r

  Right $ _st & appRslt .~ _r
