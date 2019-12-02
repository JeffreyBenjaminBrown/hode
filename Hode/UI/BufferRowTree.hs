{-# LANGUAGE LambdaCase,
ScopedTypeVariables #-}

module Hode.UI.BufferRowTree
  ( moveFocusedViewExprNode   -- ^ Direction -> St -> St
  , insertMembers_atFocus -- ^ St ->    Either String St
  , members_atFocus       -- ^ St ->    Either String (MemberFork, [Addr])
  , insertHosts_atFocus   -- ^ St ->    Either String St
  , groupHostRels_atFocus -- ^ St ->    Either String [(HostFork, [Addr])]
  , groupHostRels  -- ^ Rslt -> Addr -> Either String [(HostFork, [Addr])]
  , hostGroup_to_forkTree -- ^ Rslt -> (HostFork, [Addr]) ->
                          -- Either String (PTree ViewExprNode)
  , closeSubviews_atFocus -- ^ St -> St
  , foldSubviews_atFocus  -- ^ St -> St
  , addrsToBufferRows
    -- ^ St
    -- Set Addr -- ^ show these (can be empty) as `Addr`s,
    --            -- not (complex) `Expr`s
    -- [Addr] -- ^ each of these becomes a `BufferRow`
    -- Either String (Porest BufferRow)
  ) where

import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.List             as L
import qualified Data.List.PointedList as P
import qualified Data.Map              as M
import           Data.Set (Set)
import qualified Data.Set              as S

import           Lens.Micro hiding (has, folded)

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.UI.Types.State
import Hode.UI.Types.Views
import Hode.UI.IUtil.String
import Hode.Util.Misc
import Hode.PTree.Initial


moveFocusedViewExprNode :: Direction -> St -> St
moveFocusedViewExprNode d =
  stSetFocusedBuffer . bufferRowPorest . _Just
  %~ moveFocusInPorest d

-- TODO ? Why is this so different from `insertHosts_atFocus`?
insertMembers_atFocus :: St -> Either String St
insertMembers_atFocus st =
  prefixLeft "insertMembers_atFocus:" $ do
  let r  :: Rslt        = st ^. appRslt
      vo :: ViewOptions = st ^. viewOptions
  a            <- focusAddr st
  as :: [Addr] <- members_atFocus st

  -- The new subtree has depth two.
  -- The leaves are the second level.
  leaves :: Porest BufferRow <-
    addrsToBufferRows st (S.singleton a) as

  let new :: PTree BufferRow =
        pTreeLeaf ( BufferRow VMemberFork
                    mempty $ OtherProps False )
        & pMTrees .~ Just leaves
  Right $ st & ( stSetFocused_ViewExprNode_Tree
                 %~ consUnder_andFocus new )

members_atFocus :: St -> Either String [Addr]
members_atFocus st =
  prefixLeft "members_atFocus:" $ do
  a <- focusAddr st
  M.elems <$> has (st ^. appRslt) a

insertHosts_atFocus :: St -> Either String St
insertHosts_atFocus st =
  prefixLeft "insertHosts_atFocus:" $ do
  groups :: [(HostFork, [Addr])] <-
    groupHostRels_atFocus st
  newTrees :: [PTree BufferRow] <-
    ifLefts $ map (hostGroup_to_forkTree st) groups
  oldTrees :: Maybe (Porest BufferRow) <-
    let errMsg = "focused ViewExprNode not found."
    in maybe (Left errMsg) Right $ st ^?
       stGetFocused_ViewExprNode_Tree . _Just . pMTrees
  let oldTrees' :: [PTree BufferRow]
      oldTrees' = maybe [] toList oldTrees
      insert :: PTree BufferRow -> PTree BufferRow
      insert foc = foc & pMTrees .~
                   P.fromList (foldr (:) oldTrees' newTrees)
  Right $ st & stSetFocused_ViewExprNode_Tree %~ insert

groupHostRels_atFocus ::
  St -> Either String [(HostFork, [Addr])]
groupHostRels_atFocus st =
  prefixLeft "groupHostRels_atFocus:" $ do
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
  let ravs :: [((Role,HostAddr),ExprCtr)]
        = zip ras vs

      ( tplt_ras :: [(Role,TpltAddr)],
        rel_ras  :: [(Role,RelAddr)] ) =
        -- Separate `ras` into `Tplt`s and `Rel`s
        both %~ map fst $ L.partition isTplt ravs
        where
          isTplt :: ((Role,HostAddr),ExprCtr) -> Bool
          isTplt = (\case TpltCtr -> True; _ -> False) . snd

      maybeConsTpltHosts :: [(HostFork, [Addr])]
                         -> [(HostFork, [Addr])]
        -- There is at most one `TpltHostFork`.
        = if null tplt_ras then id
          else (:) ( TpltHostFork $ TpltHosts a0,
                     map snd tplt_ras )

  rel_tplts :: [TpltAddr] <-
    -- The `Tplt`s used by the `rel_ras`.
    -- Each implies at least one `RelHostFork`.
    let tpltOf :: RelAddr -> Either String TpltAddr
        tpltOf a = prefixLeft "tpltOf:" $
                   fills r (RoleInRel' RoleTplt, a)
    in ifLefts $ map (tpltOf . snd) rel_ras

  let rel_groups :: Map (Role,TpltAddr) [RelAddr]
      rel_groups =
        foldr f M.empty $
        (zip rel_ras rel_tplts :: [( (Role,RelAddr),
                                     TpltAddr )] )
        where
          f ::    ((Role, RelAddr), TpltAddr)
            -> Map (Role, TpltAddr) [RelAddr]
            -> Map (Role, TpltAddr) [RelAddr]
          f ((role,a),t) m =
            -- efficient: `a` is prepended, not appended.
            M.insertWith (++) (role,t) [a] m

      mkRelFork :: ((Role, TpltAddr),[RelAddr])
                -> (HostFork, [RelAddr])
      mkRelFork ((role,t),as) =
        (RelHostFork relHosts, as)
        where
          relHosts = RelHosts { _memberHostsCenter = a0
                              , _memberHostsRole = role
                              , _memberHostsTplt = tplt t }
            where tplt :: Addr -> Tplt Expr
                  tplt a = es
                    where Right (ExprTplt es) = addrToExpr r a

  Right $ maybeConsTpltHosts
    $ map mkRelFork
    $ M.toList rel_groups

-- | `hostGroup_to_forkTree st (hf, as)` makes 2-layer tree.
-- `hf` is the root.
-- The `ViewExpr`s creates from `as` form the other layer.
hostGroup_to_forkTree :: St -> (HostFork, [Addr])
                      -> Either String (PTree BufferRow)
hostGroup_to_forkTree st (hf, as) =
  prefixLeft "hostGroup_to_forkTree:" $ do
  if null as then Left "There are no host Exprs to show."
             else Right ()
  let r :: Rslt = st ^. appRslt
      vo :: ViewOptions = st ^. viewOptions
  a <- focusAddr st

  topOfNew :: BufferRow <-
    bufferRow_from_viewExprNode' st $ VHostFork hf
  leaves :: Porest BufferRow <-
    addrsToBufferRows st (S.singleton a) as
  Right $ PTree {
      _pTreeLabel    = topOfNew
    , _pTreeHasFocus = False
    , _pMTrees       = Just leaves }

closeSubviews_atFocus :: St -> St
closeSubviews_atFocus =
  stSetFocused_ViewExprNode_Tree . pMTrees .~ Nothing

foldSubviews_atFocus :: St -> St
foldSubviews_atFocus =
    ( stSetFocused_ViewExprNode_Tree . pTreeLabel
      . otherProps . folded )
    %~ not

-- | Creates a flat (depth 1) `Porest`.
-- For insertion beneath a forking `ViewExprNode`.
addrsToBufferRows
  :: St
  -> Set Addr -- ^ show these (can be empty) as `Addr`s,
              -- not (complex) `Expr`s
  -> [Addr] -- ^ each of these becomes a `BufferRow`
  -> Either String (Porest BufferRow)
addrsToBufferRows st showAsAddr as =
  prefixLeft "addrsToBufferRows:" $ do
  let r  :: Rslt        = st ^. appRslt
      vo :: ViewOptions = st ^. viewOptions
  leaves0 :: [ViewExprNode] <-
    let f = mkViewExpr r vo showAsAddr
    in map VExpr <$> ifLefts (map f as)
  leaves1 :: [BufferRow] <- ifLefts $
    map (bufferRow_from_viewExprNode' st) leaves0
  let leaves2 :: [PTree BufferRow] =
        map pTreeLeaf leaves1
  maybe (Left "Nothing to show.") Right
    $ P.fromList leaves2
