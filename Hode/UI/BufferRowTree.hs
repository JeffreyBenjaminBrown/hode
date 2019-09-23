-- | This code could be a lot shorter if I understood and/or used:
--   (1) prisms? traversals?) something to let me lens into a Vector
--   to modify it, where the return type is `Either String (Vector a)`
--   rather than `Vector a`.
--   (2) Zippers instead of Vectors. (This would obviate the first task.)
--   (2a) Mutable Vectors instead of immutable ones.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hode.UI.BufferRowTree
  ( moveFocusedViewExprNode   -- ^ Direction -> St -> St
  , members_atFocus       -- ^ St -> Either String (MembersGroup, [Addr])
  , insertMembers_atFocus -- ^ St -> Either String St
  , groupHostRels  -- ^ Rslt -> Addr -> Either String [(HostGroup, [Addr])]
  , groupHostRels_atFocus -- ^ St ->    Either String [(MemberHosts, [Addr])]
  , hostGroup_to_view     -- ^ Rslt -> (MemberHosts, [Addr]) ->
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
import Hode.UI.ITypes
import Hode.UI.String
import Hode.Util.Direction
import Hode.Util.Misc
import Hode.Util.PTree


moveFocusedViewExprNode :: Direction -> St -> St
moveFocusedViewExprNode d =
  stSetFocusedBuffer . bufferRowPorest . _Just
  %~ moveFocusInPorest d

members_atFocus :: St -> Either String (MembersGroup, [Addr])
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
  Right (MembersGroup a, as)

insertMembers_atFocus :: St -> Either String St
insertMembers_atFocus st = prefixLeft "-> insertMembers_atFocus" $ do
  (ms,as) :: (MembersGroup, [Addr]) <-
    members_atFocus st

  -- The new subtree has two levels: a top, and leaves.
  let topOfNew :: PTree BufferRow =
        pTreeLeaf $ BufferRow (VMemberGroup ms) mempty $
        OtherProps False
  leaves0 :: [ViewExprNode] <-
    map VExpr <$> ifLefts (map (resultView $ st ^. appRslt) as)
  leaves1 :: [BufferRow] <- ifLefts $
    map (bufferRow_from_viewExprNode' st) leaves0
  let leaves2 :: [PTree BufferRow] = map pTreeLeaf leaves1
  leaves3 :: Porest BufferRow <-
    let msg = "Expr has no members."
    in maybe (Left msg) Right $ P.fromList leaves2
  let new :: PTree BufferRow =
        topOfNew & pMTrees .~ Just leaves3

  Right $ st & stSetFocused_ViewExprNode_Tree %~ consUnder_andFocus new

groupHostRels :: Rslt -> Addr -> Either String [(HostGroup, [Addr])]
groupHostRels r a0 = prefixLeft "-> groupHostRels" $ do
  ras :: [(Role, Addr)] <-
    prefixLeft ("computing ras from " ++ show a0)
    $ S.toList <$> isIn r a0
  vs :: [ExprCtr] <- prefixLeft ", computing varieties"
    $ map fst
    <$> ifLefts (map (variety r . snd) ras)

  -- The rest of the work divides into grouping the `Rel`s that a0 is in,
  -- and building one group of `Tplt`s, if it's nonempty.
  let (tplt_ras :: [(Role,Addr)], rel_ras :: [(Role,Addr)]) =
        both %~ map fst $ L.partition isTplt ravs
        where ravs :: [((Role,Addr),ExprCtr)] = zip ras vs
              isTplt :: ((Role,Addr),ExprCtr) -> Bool
              isTplt = (\case TpltCtr -> True; _ -> False) . snd

  let maybeConsTpltPackage :: [(HostGroup, [Addr])] -> [(HostGroup, [Addr])]
      maybeConsTpltPackage = if null tplt_ras then id else (:) tplt_package
        where tplt_package :: (HostGroup, [Addr]) =
                (tplt_group, map snd tplt_ras)
                where tplt_group = TpltHostGroup $ JointHosts a0

  rel_tplts <- let tpltOf :: Addr -> Either String Addr
                   tpltOf a = fills r (RoleTplt, a)
               in prefixLeft "while computing rel_tplts"
                  $ ifLefts $ map (tpltOf . snd) rel_ras

  let rel_groups :: Map (Role,Addr) [Addr] -- key are (Role, Tplt) pairs
      rel_groups = foldr f M.empty $ zip rel_ras rel_tplts
        where f :: ((Role, Addr), Addr) -> Map (Role, Addr) [Addr]
                                        -> Map (Role, Addr) [Addr]
              f ((role,a),t) m = M.insertWith (++) (role,t) [a] m
          -- `f` is efficient: `a` is prepended, not appended.

      package_rel_groups :: ((Role, Addr),[Addr]) -> (HostGroup, [Addr])
      package_rel_groups ((role,t),as) = (RelHostGroup relHosts, as)
        where
          relHosts = MemberHosts { _memberHostsCenter = a0
                                 , _memberHostsRole = role
                                 , _memberHostsTplt = tplt t }
            where tplt :: Addr -> Tplt Expr
                  tplt a = es
                    where Right (ExprTplt es) = addrToExpr r a

  Right $ maybeConsTpltPackage
    $ map package_rel_groups (M.toList rel_groups)

groupHostRels_atFocus :: St -> Either String [(HostGroup, [Addr])]
groupHostRels_atFocus st = prefixLeft "-> groupHostRels_atFocus'" $ do
  a :: Addr <-
    let errMsg = "Buffer not found or focused ViewExprNode not found."
    in maybe (Left errMsg) Right $
       st ^? stGetFocused_ViewExprNode_Tree . _Just .
       pTreeLabel . viewExprNode . _VExpr . viewExpr_Addr
  groupHostRels (st ^. appRslt) a

insertHosts_atFocus :: St -> Either String St
insertHosts_atFocus st = prefixLeft "-> insertHosts_atFocus" $ do
  (groups :: [(HostGroup, [Addr])]) <-
    groupHostRels_atFocus st
  (newTrees :: [PTree BufferRow]) <-
    ifLefts $ map (hostGroup_to_view st) groups
  (preexist :: Maybe (Porest BufferRow)) <-
    let errMsg = "focused ViewExprNode not found."
    in maybe (Left errMsg) Right $ st ^?
       stGetFocused_ViewExprNode_Tree . _Just . pMTrees
  let preexist' :: [PTree BufferRow]
      preexist' = maybe [] toList preexist
      insert :: PTree BufferRow -> PTree BufferRow
      insert foc = foc & pMTrees .~
                   P.fromList (foldr (:) preexist' newTrees)
  Right $ st & stSetFocused_ViewExprNode_Tree %~ insert

hostGroup_to_view :: St -> (HostGroup, [Addr])
                  -> Either String (PTree BufferRow)
hostGroup_to_view st (hg, as) = prefixLeft "-> hostGroup_to_view" $ do
  case as of [] -> Left "There are no host Exprs to show."
             _  -> Right ()
  let mustBeOkay = "Impossible: `as` is nonempty, so P.fromList must work."
      r = st ^. appRslt
  rs :: [ViewExpr] <-
    ifLefts $ map (resultView r) as

  -- The new subtree has two layers: the top and the leaves
  topOfNew <- bufferRow_from_viewExprNode' st $ VHostGroup hg
  let leaves0 :: [ViewExprNode] = map VExpr rs
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
