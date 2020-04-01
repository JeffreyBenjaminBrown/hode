{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.ExprTree.Sort (
  sortFocusAndPeers -- ^ (BinOrientation, TpltAddr) -> St -> Either String St
  ) where

import           Control.Lens hiding (has, folded)
import           Data.Foldable (toList)
import qualified Data.List             as L
import           Data.Maybe
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
addSelections_toSortedRegion st =
  prefixLeft "addSelections_toSortedRegion: " $ do
  peers :: Porest ExprRow <- stFocusPeers st
  (bo,t) :: (BinOrientation, TpltAddr) <-
    let errMsg = "Focused node and its peers have not been sorted."
    in maybe (Left errMsg) Right
       $ st ^? ( stGet_focusedBuffer . bufferExprRowTree .
                 pTreeLabel . otherProps . childSort . _Just )
  let peerErs :: [ExprRow] =
        map (^. pTreeLabel) $ toList peers
      (inSort :: [ExprRow], outSort :: [ExprRow]) =
        L.partition (^. boolProps . inSortGroup) peerErs
      (seld :: [ExprRow], unseld :: [ExprRow]) =
        L.partition (^. boolProps . selected) outSort
  if not $ null seld then Right ()
    else Left "Nothing has been selected."
  let inSortAs :: [Addr] = inSort ^.. traversed . exprRow_addr
      seldAs   :: [Addr] = seld   ^.. traversed . exprRow_addr
      unseldAs :: [Addr] = unseld ^.. traversed . exprRow_addr
      as       :: [Addr] = inSortAs ++ seldAs ++ unseldAs
      
  r :: Rslt <- insertChain (bo,t) seldAs $ st ^. appRslt
  r :: Rslt <- case lastOf traversed inSortAs of
    Nothing -> Right r
    Just (a :: Addr) ->
      -- connect (last of old sorted) to (first of new sorted)
      let re :: RefExpr = case bo of
            LeftEarlier  -> Rel' $ Rel [a,head seldAs] t
            RightEarlier -> Rel' $ Rel [head seldAs,a] t
      in insert re r
  error ""

-- peerErs :: [ExprRow] <-
--    let (nulls,justs) = L.partition isNothing peerMAddrs
--    in if null nulls then Right justs
--       else Left "peer `ExprRow`s should all contain `VenExpr`s, but don't."

-- sortPList_asList
--   (maybe (error "impossible") id . (^? pTreeLabel . xprRow_addr))
--   order peers
