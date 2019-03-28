-- | This code could be a lot shorter if I understood and/or used:
-- (1) prisms? traversals?) something to let me lens into a Vector
-- to modify it, where the return type is `Either String (Vector a)`
-- rather than `Vector a`.
-- (2) Zippers instead of Vectors. (This would obviate the first task.)
-- (2a) Mutable Vectors instead of immutable ones.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UI.RsltViewTree (
    moveFocusedRsltView -- Direction -> St -> Either String St
  , moveFocusedRsltView_puffer -- ^ Direction -> St -> St
  , members_atFocus         -- St -> Either String (ViewMembers, [Addr])
  , members_atFocus_puffer -- ^ St -> Either String (ViewMembers, [Addr])
  , insertMembers_atFocus   -- St -> Either String St
  , groupHostRels -- Rslt -> Addr -> Either String [(ViewCenterRole, [Addr])]
  , groupHostRels_atFocus   -- St -> Either String [(ViewCenterRole, [Addr])]
  , hostRelGroup_to_view -- Rslt -> (ViewCenterRole, [Addr])
                         -- -> Either String (VTree RsltView)
  , insertHosts_atFocus    -- St -> Either String St
  , closeSubviews_atFocus  -- St -> Either String St
  ) where

import           Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Vector as V

import           Lens.Micro hiding (has)

import Rslt.RLookup
import Rslt.RTypes
import UI.ITypes
import UI.String
import Util.Direction
import Util.Misc
import Util.PTree
import Util.VTree


-- TODO ? some clever `Traversal` work could probably make
-- `moveFocusedRsltView` into a one-liner
moveFocusedRsltView :: Direction -> St -> Either String St
moveFocusedRsltView d st = prefixLeft "moveFocusedRsltView" $ do
  b <- maybe (Left "Bad vathToBuffer in St.") Right
    $ st ^? stBuffer st
  (p :: Path, vt :: VTree RsltView) <- moveFocusInTree d
    (b ^. bufferPath, b ^. bufferView)
  Right $ st & stBuffer st . bufferView .~ vt
             & stBuffer st . bufferPath .~ p

moveFocusedRsltView_puffer :: Direction -> St -> St
moveFocusedRsltView_puffer d st =
  st & stSetPuffer . pufferView %~ moveFocusInPTree d

members_atFocus :: St -> Either String (ViewMembers, [Addr])
members_atFocus st = prefixLeft "members_atFocus" $ do
  (b :: Buffer) <- let msg = "bad vathToBuffer"
    in maybe (Left msg) Right $ st ^? stBuffer st
  let (viewPath :: Path) = b ^. bufferPath
  (foc :: VTree RsltView) <- let left = Left $ "bad path: " ++ show viewPath
    in maybe left Right $ b ^? bufferView . atPath viewPath
  (a :: Addr) <- case foc ^. vTreeLabel of
    VResult rv -> Right $ rv ^. viewResultAddr
    _ -> Left $ "can only be called from a RsltView with an Addr."
  (as :: [Addr]) <- M.elems <$> has (st ^. appRslt) a
  Right ( ViewMembers a, as )

members_atFocus_puffer :: St -> Either String (ViewMembers, [Addr])
members_atFocus_puffer st = prefixLeft "members_atFocus" $ do
  (foc :: PTree RsltView) <- let msg = "focused RsltView not found."
    in maybe (error msg) Right $
       st ^? stGetPuffer . _Just . pufferView . getFocusedSubtree . _Just
  (a :: Addr) <- case foc ^. pTreeLabel of
    VResult rv -> Right $ rv ^. viewResultAddr
    _ -> Left $ "can only be called from a RsltView with an Addr."
  (as :: [Addr]) <- M.elems <$> has (st ^. appRslt) a
  Right (ViewMembers a, as)

insertMembers_atFocus :: St -> Either String St
insertMembers_atFocus st = prefixLeft "insertMembers_atFocus" $ do
  ((ms,as) :: (ViewMembers, [Addr])) <- members_atFocus st
  let (topOfNew :: VTree RsltView) = vTreeLeaf $ VMembers ms
  (leavesOfNew :: [VTree RsltView]) <- map (vTreeLeaf . VResult)
    <$> ifLefts "" (map (resultView (st ^. appRslt)) as)
  let (new :: VTree RsltView) =
        topOfNew & vTrees .~ V.fromList leavesOfNew
  (b :: Buffer) <- let msg = "stBuffer returned Nothing."
                   in maybe (Left msg) Right $ st ^? stBuffer st
  vt' <- consUnderFocus (b ^. bufferPath) new (b ^. bufferView)
  Right $ st & stBuffer st . bufferView .~ vt'


groupHostRels :: Rslt -> Addr -> Either String [(ViewCenterRole, [Addr])]
groupHostRels r a0 = do
  (ras :: [(Role, Addr)]) <- let
    msg = "groupHostRels, computing ras from center " ++ show a0
    in prefixLeft msg $ S.toList <$> isIn r a0
  (ts :: [Addr]) <- let
    tpltAddr :: Addr -> Either String Addr
    tpltAddr a = prefixLeft msg $ fills r (RoleTplt, a)
      where msg = "groupHostRels, computing tpltAddr of " ++ show a
    in ifLefts "" $ map (tpltAddr . snd) ras
  let groups :: Map (Role,Addr) [Addr] -- key are (Role, Tplt) pairs
      groups = foldr f M.empty $ zip ras ts where
        f :: ((Role, Addr), Addr) -> Map (Role, Addr) [Addr]
                                  -> Map (Role, Addr) [Addr]
        f ((role,a),t) m = M.insertWith (++) (role,t) [a] m
      package :: ((Role, Addr),[Addr]) -> (ViewCenterRole, [Addr])
      package ((role,t),as) = (crv, as) where
        crv = ViewCenterRole { _crvCenter = a0
                             , _crvRole = role
                             , _crvTplt = tplt t } where
          tplt :: Addr -> [Expr]
          tplt a = es where Right (Tplt es) = addrToExpr r a
  Right $ map package $ M.toList groups


groupHostRels_atFocus :: St -> Either String [(ViewCenterRole, [Addr])]
groupHostRels_atFocus st = prefixLeft "groupHostRels_atFocus'" $ do
  let noBuffer = Left "Cannot be done when there is no buffer."
  (top :: VTree RsltView) <- maybe noBuffer Right
    $ st ^? stBuffer st . bufferView
  (p   :: Path)           <- maybe noBuffer Right
    $ st ^? stBuffer st . bufferPath
  _ <- pathInBounds top p
  a :: Addr <-
    let err = Left "Either target RsltView has no Addr, or bad bufferPath."
    in maybe err Right
       $ top ^? atPath p . vTreeLabel . _VResult . viewResultAddr
  groupHostRels (st ^. appRslt) a


insertHosts_atFocus :: St -> Either String St
insertHosts_atFocus st = prefixLeft "insertHosts_atFocus" $ do
  (groups :: [(ViewCenterRole, [Addr])]) <-
    groupHostRels_atFocus st
  (newTrees :: [VTree RsltView]) <- ifLefts ""
    $ map (hostRelGroup_to_view $ st ^. appRslt) groups
  let insert :: VTree RsltView -> VTree RsltView
      insert vt = vt & vTrees .~
                  V.fromList (foldr (:) preexist newTrees) where
        (preexist :: [VTree RsltView]) =  V.toList $ vt ^. vTrees
  Right $ st & stBuffer st . bufferView
    . atPath (st ^. stBuffer st . bufferPath) %~ insert


hostRelGroup_to_view :: Rslt -> (ViewCenterRole, [Addr])
                     -> Either String (VTree RsltView)
hostRelGroup_to_view r (crv, as) = do
  (rs :: [ViewResult]) <- ifLefts "hostRelGroup_to_view"
    $ map (resultView r) as
  Right $ VTree { _vTreeFocalChild = 0
                , _vTreeIsFocused = False
                , _vTreeLabel = VCenterRole crv
                , _vTrees =
                    V.fromList $ map (vTreeLeaf . VResult) rs }


closeSubviews_atFocus :: St -> Either String St
closeSubviews_atFocus st = st & prefixLeft "closeSubviews_atFocus"
  . eitherIntoTraversal (stBuffer st) _closeSubviews_atFocus

_closeSubviews_atFocus :: Buffer -> Either String Buffer
_closeSubviews_atFocus b = do
  _ <- pathInBounds (b ^. bufferView) (b ^. bufferPath)
  _ <- let err = "Closing the root of a view would be silly."
       in if b ^. bufferPath == [] then Left err else Right ()
  let close :: VTree RsltView -> VTree RsltView
      close = vTrees .~ mempty
  Right $ b & bufferView . atPath (b ^. bufferPath) %~ close
