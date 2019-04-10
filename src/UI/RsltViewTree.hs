-- | This code could be a lot shorter if I understood and/or used:
-- (1) prisms? traversals?) something to let me lens into a Vector
-- to modify it, where the return type is `Either String (Vector a)`
-- rather than `Vector a`.
-- (2) Zippers instead of Vectors. (This would obviate the first task.)
-- (2a) Mutable Vectors instead of immutable ones.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.RsltViewTree (
    moveFocusedRsltView   -- ^ Direction -> St -> St
  , members_atFocus       -- ^ St -> Either String (ViewMembers, [Addr])
  , insertMembers_atFocus -- ^ St -> Either String St
  , groupHostRels  -- ^ Rslt -> Addr -> Either String [(HostGroup, [Addr])]
  , groupHostRels_atFocus -- ^ St ->    Either String [(MemberHosts, [Addr])]
  , hostGroup_to_view     -- ^ Rslt -> (MemberHosts, [Addr]) ->
                          -- Either String (PTree RsltView)
  , insertHosts_atFocus   -- ^ St -> Either String St
  , closeSubviews_atFocus -- ^ St -> St
  ) where

import           Data.Foldable (toList)
import           Data.Map (Map)
import qualified Data.List             as L
import qualified Data.List.PointedList as P
import qualified Data.Map              as M
import qualified Data.Set              as S

import           Lens.Micro hiding (has)

import Rslt.RLookup
import Rslt.RTypes
import UI.ITypes
import UI.String
import Util.Direction
import Util.Misc
import Util.PTree


moveFocusedRsltView :: Direction -> St -> St
moveFocusedRsltView d st =
  st & stSetFocusedBuffer . bufferRsltViewPorest . _Just
     %~ moveFocusInPorest d

members_atFocus :: St -> Either String (ViewMembers, [Addr])
members_atFocus st = prefixLeft "members_atFocus" $ do
  foc :: PTree RsltView <-
    let msg = "focused RsltView not found."
    in maybe (error msg) Right $
       st ^? stGetFocusedRsltViewTree . _Just
  a :: Addr <-
    case foc ^. pTreeLabel of
      VResult rv -> Right $ rv ^. viewResultAddr
      _          -> Left $
        "can only be called from a RsltView with an Addr."
  as :: [Addr] <-
    M.elems <$> has (st ^. appRslt) a
  Right (ViewMembers a, as)

insertMembers_atFocus :: St -> Either String St
insertMembers_atFocus st = prefixLeft "insertMembers_atFocus" $ do
  (ms,as) :: (ViewMembers, [Addr]) <-
    members_atFocus st
  let topOfNew :: PTree RsltView =
        pTreeLeaf $ VMembers ms
  leavesOfNew :: [PTree RsltView] <-
    map (pTreeLeaf . VResult)
    <$> ifLefts "" (map (resultView $ st ^. appRslt) as)
  leavesOfNew' :: Porest RsltView <-
    let msg = "Expr has no members."
    in maybe (Left msg) Right $ P.fromList leavesOfNew
  let new :: PTree RsltView =
        topOfNew & pMTrees .~ Just leavesOfNew'
  Right $ st & stSetFocusedRsltViewTree %~ consUnderAndFocus new

groupHostRels :: Rslt -> Addr -> Either String [(HostGroup, [Addr])]
groupHostRels r a0 = prefixLeft "groupHostRels" $ do
  ras :: [(Role, Addr)] <- let
    msg = "computing ras from " ++ show a0
    in prefixLeft msg $ S.toList <$> isIn r a0
  vs :: [ExprCtr] <- map fst <$>
    ( ifLefts "while computing varieties" $
      map (variety r . snd) ras )

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
                where tplt_group = TpltHostGroup $ Templates a0

  rel_tplts <- let tpltOf :: Addr -> Either String Addr
                   tpltOf a = fills r (RoleTplt, a)
               in ifLefts "while computing rel_tplts" $
                  map (tpltOf . snd) rel_ras

  let rel_groups :: Map (Role,Addr) [Addr] -- key are (Role, Tplt) pairs
      rel_groups = foldr f M.empty $ zip rel_ras rel_tplts
        where f :: ((Role, Addr), Addr) -> Map (Role, Addr) [Addr]
                                        -> Map (Role, Addr) [Addr]
              f ((role,a),t) m = M.insertWith (++) (role,t) [a] m
          -- `f` is efficient: `a` is prepended, not appended.

      package_rel_groups :: ((Role, Addr),[Addr]) -> (HostGroup, [Addr])
      package_rel_groups ((role,t),as) = (RelHostGroup relHosts, as)
        where
          relHosts = MemberHosts { _relHostsCenter = a0
                                 , _relHostsRole = role
                                 , _relHostsTplt = tplt t }
            where tplt :: Addr -> [Expr]
                  tplt a = es
                    where Right (ExprTplt es) = addrToExpr r a

  Right $ maybeConsTpltPackage
    $ map package_rel_groups (M.toList rel_groups)

groupHostRels_atFocus :: St -> Either String [(HostGroup, [Addr])]
groupHostRels_atFocus st = prefixLeft "groupHostRels_atFocus'" $ do
  a :: Addr <-
    let errMsg = "Buffer not found or focused RsltView not found."
    in maybe (Left errMsg) Right $
       st ^? stGetFocusedRsltViewTree . _Just .
       pTreeLabel . _VResult . viewResultAddr
  groupHostRels (st ^. appRslt) a

insertHosts_atFocus :: St -> Either String St
insertHosts_atFocus st = prefixLeft "insertHosts_atFocus" $ do
  (groups :: [(HostGroup, [Addr])]) <-
    groupHostRels_atFocus st
  (newTrees :: [PTree RsltView]) <- ifLefts ""
    $ map (hostGroup_to_view $ st ^. appRslt) groups
  (preexist :: Maybe (Porest RsltView)) <-
    let errMsg = "focused RsltView not found."
    in maybe (Left errMsg) Right $ st ^?
       stGetFocusedRsltViewTree . _Just . pMTrees
  let (preexist' :: [PTree RsltView]) = maybe [] toList preexist
      insert :: PTree RsltView -> PTree RsltView
      insert foc = foc & pMTrees .~
                  P.fromList (foldr (:) preexist' newTrees)
  Right $ st & stSetFocusedRsltViewTree %~ insert

hostGroup_to_view :: Rslt -> (HostGroup, [Addr])
                     -> Either String (PTree RsltView)
hostGroup_to_view r (hg, as) = do
  case as of [] -> Left "There are no host Exprs to show."
             _ -> Right ()
  let mustBeOkay = "Impossible: as is nonempty, so P.fromList must work."
  (rs :: [ViewResult]) <- ifLefts "hostGroup_to_view"
    $ map (resultView r) as
  Right $ PTree { _pTreeLabel = VHostGroup hg
                , _pTreeHasFocus = False
                , _pMTrees = maybe (error mustBeOkay) Just $
                    P.fromList $ map (pTreeLeaf . VResult) rs }

closeSubviews_atFocus :: St -> St
closeSubviews_atFocus =
  stSetFocusedRsltViewTree . pMTrees .~ Nothing
