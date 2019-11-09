{-# LANGUAGE LambdaCase,
ScopedTypeVariables #-}

module Hode.UI.BufferRowTree
  ( moveFocusedViewExprNode   -- ^ Direction -> St -> St
  , members_atFocus       -- ^ St -> Either String (MemberFork, [Addr])
  , insertMembers_atFocus -- ^ St -> Either String St
  , groupHostRels  -- ^ Rslt -> Addr -> Either String [(HostFork, [Addr])]
  , groupHostRels_atFocus -- ^ St ->    Either String [(RelHosts, [Addr])]
  , hostGroup_to_view     -- ^ Rslt -> (RelHosts, [Addr]) ->
                          -- Either String (PTree ViewExprNode)
  , insertHosts_atFocus   -- ^ St -> Either String St
  , closeSubviews_atFocus -- ^ St -> St
  , foldSubviews_atFocus  -- ^ St -> St
  ) where

import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.List             as L
import qualified Data.List.PointedList as P
import qualified Data.Map              as M
import qualified Data.Set              as S

import           Lens.Micro hiding (has, folded)

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.IUtil.String
import Hode.Util.Direction
import Hode.Util.Misc
import Hode.Util.PTree


moveFocusedViewExprNode :: Direction -> St -> St
moveFocusedViewExprNode d =
  stSetFocusedBuffer . bufferRowPorest . _Just
  %~ moveFocusInPorest d

members_atFocus :: St -> Either String (MemberFork, [Addr])
members_atFocus st = prefixLeft "-> members_atFocus" $ do
  foc :: PTree BufferRow <-
    let msg = "focused ViewExprNode not found."
    in maybe (error msg) Right $
       st ^? stGetFocused_ViewExprNode_Tree . _Just
  a :: Addr <-
    case foc ^. pTreeLabel . viewExprNode of
      VExpr rv -> Right $ rv ^. viewExpr_Addr
      _        -> Left $ "can only be called from a ViewExprNode with an Addr."
  as :: [Addr] <-
    M.elems <$> has (st ^. appRslt) a
  Right (MemberFork a, as)

insertMembers_atFocus :: St -> Either String St
insertMembers_atFocus st = prefixLeft "-> insertMembers_atFocus" $ do
  (ms,as) :: (MemberFork, [Addr]) <-
    members_atFocus st

  -- The new subtree has two levels: a top, and leaves.
  let topOfNew :: PTree BufferRow =
        pTreeLeaf $ BufferRow (VMemberFork ms) mempty $
        OtherProps False
  leaves0 :: [ViewExprNode] <-
    map VExpr <$> ifLefts (map (mkViewExpr $ st ^. appRslt) as)
  leaves1 :: [BufferRow] <- ifLefts $
    map (bufferRow_from_viewExprNode' st) leaves0
  let leaves2 :: [PTree BufferRow] = map pTreeLeaf leaves1
  leaves3 :: Porest BufferRow <-
    let msg = "Expr has no members."
    in maybe (Left msg) Right $ P.fromList leaves2
  let new :: PTree BufferRow =
        topOfNew & pMTrees .~ Just leaves3

  Right $ st & stSetFocused_ViewExprNode_Tree %~ consUnder_andFocus new

insertHosts_atFocus :: St -> Either String St
insertHosts_atFocus st =
  prefixLeft "insertHosts_atFocus:" $ do
  groups :: [(HostFork, [Addr])] <-
    groupHostRels_atFocus st
  newTrees :: [PTree BufferRow] <-
    ifLefts $ map (hostGroup_to_view st) groups
  preexist :: Maybe (Porest BufferRow) <-
    let errMsg = "focused ViewExprNode not found."
    in maybe (Left errMsg) Right $ st ^?
       stGetFocused_ViewExprNode_Tree . _Just . pMTrees
  let preexist' :: [PTree BufferRow]
      preexist' = maybe [] toList preexist
      insert :: PTree BufferRow -> PTree BufferRow
      insert foc = foc & pMTrees .~
                   P.fromList (foldr (:) preexist' newTrees)
  Right $ st & stSetFocused_ViewExprNode_Tree %~ insert

groupHostRels_atFocus ::
  St -> Either String [(HostFork, [Addr])]
groupHostRels_atFocus st =
  prefixLeft "-> groupHostRels_atFocus'" $ do
  a :: Addr <- maybe
    (Left "Buffer or focused ViewExprNode not found.")
    Right $ st ^? stGetFocused_ViewExprNode_Tree . _Just .
      pTreeLabel . viewExprNode . _VExpr . viewExpr_Addr
  groupHostRels (st ^. appRslt) a

groupHostRels ::
  Rslt -> Addr -> Either String [(HostFork, [Addr])]
groupHostRels r a0 =
  prefixLeft "groupHostRels:" $ do
  ras :: [(Role, HostAddr)] <-
    S.toList <$> isIn r a0
  vs :: [ExprCtr] <-
    map fst <$> ifLefts (map (variety r . snd) ras)

  -- Separate `ras` into `Tplt`s and `Rel`s
  let ( tplt_ras :: [(Role,TpltAddr)],
        rel_ras  :: [(Role,RelAddr)] ) =
        both %~ map fst $ L.partition isTplt ravs
        where
          ravs :: [((Role,HostAddr),ExprCtr)] = zip ras vs
          isTplt :: ((Role,HostAddr),ExprCtr) -> Bool
          isTplt = (\case TpltCtr -> True; _ -> False) . snd

  -- There might not be any `Tplt` hosts to add.
  let maybeConsTpltHosts ::
        [(HostFork, [Addr])] -> [(HostFork, [Addr])]
      maybeConsTpltHosts =
        if null tplt_ras then id else (:) tpltHosts
        where tpltHosts :: (HostFork, [Addr]) =
                (tplt_fork, map snd tplt_ras)
                where tplt_fork = TpltHostFork $ TpltHosts a0

  -- The rest of the work divides into grouping the `Rel`s that a0 is in,
  -- and building one group of `Tplt`s, if it's nonempty.
  rel_tplts <- let tpltOf :: Addr -> Either String Addr
                   tpltOf a = fills r (RoleInRel' RoleTplt, a)
               in prefixLeft "while computing rel_tplts"
                  $ ifLefts $ map (tpltOf . snd) rel_ras

  let rel_groups :: Map (Role,Addr) [Addr]
      rel_groups =
        foldr f M.empty $ zip rel_ras rel_tplts
        where f ::    ((Role, Addr), Addr)
                -> Map (Role, Addr) [Addr]
                -> Map (Role, Addr) [Addr]
              f ((role,a),t) m =
                M.insertWith (++) (role,t) [a] m
          -- `f` is efficient: `a` is prepended, not appended.

      package_rel_groups ::
        ((Role, Addr),[Addr]) -> (HostFork, [Addr])
      package_rel_groups ((role,t),as) =
        (RelHostFork relHosts, as)
        where
          relHosts = RelHosts { _memberHostsCenter = a0
                               , _memberHostsRole = role
                               , _memberHostsTplt = tplt t }
            where tplt :: Addr -> Tplt Expr
                  tplt a = es
                    where Right (ExprTplt es) = addrToExpr r a

  Right $ maybeConsTpltHosts
    $ map package_rel_groups (M.toList rel_groups)

-- | `hostGroup_to_view st (hg, as)` makes 2-layer tree.
-- `hg` is the root.
-- The `ViewExpr`s creates from `as` form the other layer.
hostGroup_to_view :: St -> (HostFork, [Addr])
                  -> Either String (PTree BufferRow)
hostGroup_to_view st (hg, as) =
  prefixLeft "-> hostGroup_to_view" $ do
  if null as then Left "There are no host Exprs to show."
             else Right ()
  let mustBeOkay = "Impossible: `as` is nonempty, "
                   ++ "so P.fromList must work."
      r = st ^. appRslt
  ves :: [ViewExpr] <-
    ifLefts $ map (mkViewExpr r) as

  topOfNew :: BufferRow <-
    bufferRow_from_viewExprNode' st $ VHostFork hg
  let leaves0 :: [ViewExprNode] = map VExpr ves
  leaves1 :: [BufferRow] <- ifLefts $
    map (bufferRow_from_viewExprNode' st) leaves0
  Right $ PTree {
      _pTreeLabel    = topOfNew
    , _pTreeHasFocus = False
    , _pMTrees       = maybe (error mustBeOkay) Just $
                       P.fromList $ map pTreeLeaf leaves1 }

closeSubviews_atFocus :: St -> St
closeSubviews_atFocus =
  stSetFocused_ViewExprNode_Tree . pMTrees .~ Nothing

foldSubviews_atFocus :: St -> St
foldSubviews_atFocus =
    ( stSetFocused_ViewExprNode_Tree . pTreeLabel
      . otherProps . folded )
    %~ not
