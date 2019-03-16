-- | This code could be a lot shorter if I understood and/or used:
-- (1) prisms? traversals?) something to let me lens into a Vector
-- to modify it, where the return type is `Either String (Vector a)`
-- rather than `Vector a`.
-- (2) Zippers instead of Vectors. (This would obviate the first task.)
-- (2a) Mutable Vectors instead of immutable ones.


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module UI.ViewTree (
    pathInBounds -- ViewTree -> Path  -> Either String ()
  , atPath       -- Path -> Traversal' ViewTree ViewTree
  , getViewTreeAt -- [Int] -> ViewTree -> Either String ViewTree
  , modViewTreeAt -- [Int] -> (ViewTree -> ViewTree) -> ViewTree
                                    -- -> Either String ViewTree
  , moveFocus  -- Direction -> St -> Either String St
  , groupHostRels -- Rslt -> Addr -> Either String [(CenterRoleView, [Addr])]
  , resultView    -- Rslt -> Addr -> Either String ResultView
  , hostRelGroup_to_view -- Rslt -> (CenterRoleView, [Addr])
                         -- -> Either String ViewTree
  , insertHosts -- St -> Either String St
  ) where

import           Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Control.Lens.Combinators (from)
import           Data.Vector.Lens (vector)
import           Lens.Micro

import Rslt.RLookup
import Rslt.RTypes
import UI.ITypes
import UI.IUtil
import Util.Misc


pathInBounds :: ViewTree -> Path -> Either String ()
pathInBounds _ [] = Right ()
pathInBounds vt (p:ps) = let vs = vt ^. viewSubviews
  in case inBounds vs p of
       True -> pathInBounds (vs V.! p) ps
       False -> Left "pathInBounds: It's not."


atPath :: Path -> Traversal' ViewTree ViewTree
atPath [] = lens id $ flip const -- the trivial lens
atPath (p:ps) = viewSubviews . from vector
                . ix p . atPath ps


-- TODO : The next two functions should be (prismatic?) one-liners.
-- TODO : The `Vector ViewTree` field really ought (so far) to be a zipper.
getViewTreeAt :: Path -> ViewTree -> Either String ViewTree
getViewTreeAt [] v = Right v
getViewTreeAt (p:path) v = do
  let (subvs :: V.Vector ViewTree) = v ^. viewSubviews
  _ <- let errMsg = "getViewTreeAt: index " ++ show p ++ " out of bounds."
       in if inBounds subvs p then Right () else Left errMsg
  let subv = (V.!) subvs p
  getViewTreeAt path subv


modViewTreeAt :: Path -> (ViewTree -> ViewTree) -> ViewTree -> Either String ViewTree
modViewTreeAt []       f v = Right $ f v
modViewTreeAt (p:path) f v = do
  let (subvs :: V.Vector ViewTree) = v ^. viewSubviews
  _ <- let errMsg = "getViewTreeAt: index " ++ show p ++ " out of bounds."
       in if inBounds subvs p then Right () else Left errMsg
  let subv = (V.!) subvs p
  subv' <- modViewTreeAt path f subv
  let Just subvs' = modifyAt p (const subv') subvs
      -- it's not `Nothing` because I already checked `inBounds`.
  Right $ v & viewSubviews .~ subvs'


moveFocus :: Direction -> St -> Either String St
moveFocus DirLeft st@( _pathToFocus -> [] ) = Right st
moveFocus DirLeft st = Right $ st & pathToFocus %~ tail

moveFocus DirRight st = do
  (v :: ViewTree) <- prefixLeft "moveFocus"
    $ getViewTreeAt (st ^. pathToFocus)
    (st ^. viewTree)
  case null $ v ^. viewSubviews of
    True -> Right st
    False -> let nextFocusLink = v ^. viewFocus
      in Right $ st & pathToFocus %~ (++ [nextFocusLink])

moveFocus DirUp st = do
  let path = st ^. pathToFocus
      pathToParent = take (length path - 1) path
      topView = st ^. viewTree
  (parent :: ViewTree) <- prefixLeft "moveFocus, computing parent"
    $ getViewTreeAt pathToParent topView
  let parFoc = parent ^. viewFocus
      parFoc' = if inBounds (parent ^. viewSubviews) parFoc
        then max 0 $ parFoc - 1
        else 0
  path' <- prefixLeft "moveFocus, computing path'"
           $ replaceLast parFoc' path
  topView' <- modViewTreeAt pathToParent
              (viewFocus .~ parFoc') topView
  Right $ st & pathToFocus .~ path'
    & viewTree .~ topView'

moveFocus DirDown st = do
  let path = st ^. pathToFocus
      pathToParent = take (length path - 1) path
      topView = st ^. viewTree
  (parent :: ViewTree) <- prefixLeft "moveFocus"
    $ getViewTreeAt pathToParent topView
  let parFoc = parent ^. viewFocus
      parFoc' = if inBounds (parent ^. viewSubviews) parFoc
        then min (parFoc + 1) $ V.length (parent ^. viewSubviews) - 1
        else 0
  path' <- prefixLeft "moveFocus, computing path'"
           $ replaceLast parFoc' path
  topView' <- modViewTreeAt pathToParent
              (viewFocus .~ parFoc') topView
  Right $ st & pathToFocus .~ path'
    & viewTree .~ topView'


groupHostRels :: Rslt -> Addr -> Either String [(CenterRoleView, [Addr])]
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
      package :: ((Role, Addr),[Addr]) -> (CenterRoleView, [Addr])
      package ((role,t),as) = (crv, as) where
        crv = CenterRoleView { _crvCenter = a0
                             , _crvRole = role
                             , _crvTplt = tplt t } where
          tplt :: Addr -> [Expr]
          tplt a = es where Right (Tplt es) = addrToExpr r a
  Right $ map package $ M.toList groups


insertHosts :: St -> Either String St
insertHosts st = prefixLeft "insertHosts" $ do
  let (p :: Path) = st ^. pathToFocus
      (top :: ViewTree) = st ^. viewTree
      (r :: Rslt) = st ^. appRslt
  (foc :: ViewTree) <- getViewTreeAt p top
  let (focView :: View) = foc ^. viewContent
  case focView of VResult _ -> Right ()
                  _ -> Left $ "target View must have an Addr"
  let VResult (rv :: ResultView) = focView
      (focAddr :: Addr) = rv ^. viewResultAddr
  (hostRelGroups :: [(CenterRoleView, [Addr])]) <-
    groupHostRels r focAddr
  (newTrees :: [ViewTree]) <-
    ifLefts "" $ map (hostRelGroup_to_view r) hostRelGroups
  let newFoc = foc & viewSubviews %~ f
        where f :: Vector ViewTree -> Vector ViewTree
              f vt = foldr (flip V.snoc) vt newTrees
  newTop <- modViewTreeAt p (const newFoc) top
  Right $ st & viewTree .~ newTop


hostRelGroup_to_view :: Rslt -> (CenterRoleView, [Addr])
                     -> Either String ViewTree
hostRelGroup_to_view r (crv, as) = do
  (rs :: [ResultView]) <- ifLefts "hostRelGroup_to_view"
    $ map (resultView r) as
  Right $ ViewTree { _viewFocus = 0
                   , _viewIsFocused = False
                   , _viewContent = VCenterRoleView crv
                   , _viewSubviews =
                     V.fromList $ map (viewLeaf . VResult) rs }
