{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.ExprTree.Sort (
  sortFocusAndPeers -- ^ (BinOrientation, TpltAddr) -> St -> Either String St
  ) where

import           Control.Lens hiding (has, folded)
import           Data.Foldable (toList)
import qualified Data.List             as L
import           Data.Set (Set)
import qualified Data.Set              as S

import Hode.PTree
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.Rslt.Sort
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
  let r :: Rslt = st ^. appRslt

  peers :: Porest ExprRow <-
    case stFocusPeers st of
      Nothing -> Left $ "Sort failed. Probably because the focused node is the root of the view, so it has no peers to sort."
      Just x -> Right x
  let pTreeExprRow_toAddr :: PTree ExprRow -> Maybe Addr =
        (^? pTreeLabel . viewExprNode . _VenExpr . viewExpr_Addr)
      mas :: [Maybe Addr] = map pTreeExprRow_toAddr $ toList peers
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
        (maybe (error "impossible") id . pTreeExprRow_toAddr)
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
