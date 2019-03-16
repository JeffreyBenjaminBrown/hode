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
  , moveFocus  -- Direction -> St -> Either String St
  , groupHostRels -- Rslt -> Addr -> Either String [(CenterRoleView, [Addr])]
  , hostRelGroup_to_view -- Rslt -> (CenterRoleView, [Addr])
                         -- -> Either String ViewTree
  , insertHosts  -- St -> Either String St
  ) where

import           Data.Map (Map)
import qualified Data.Map    as M
import qualified Data.Set    as S
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


moveFocus :: Direction -> St -> Either String St
moveFocus DirLeft st@( _pathToFocus -> [] ) = Right st
moveFocus DirLeft st = Right $ st & pathToFocus %~ tail

moveFocus DirRight st = do
  foc <- let err = "moveFocus: bad focus "
                   ++ show (st ^. pathToFocus)
         in maybe (Left err) Right
            $ (st ^. viewTree) ^? atPath (st ^. pathToFocus)
  if null $ foc ^. viewSubviews
    then Right st
    else Right $ st & pathToFocus %~ (++ [foc ^. viewFocus])

moveFocus DirUp st = do
  let topView = st ^. viewTree
      path = st ^. pathToFocus
  _ <- pathInBounds topView path
  let pathToParent = take (length path - 1) path
      Just parent = -- safe b/c path is in bounds
        topView ^? atPath pathToParent
      parFoc = parent ^. viewFocus
  _ <- if inBounds (parent ^. viewSubviews) parFoc then Right ()
       else Left $ "Bad focus in " ++ show parent
            ++ " at " ++ show pathToParent
  let parFoc' = max 0 $ parFoc - 1
  Right $ st
        & pathToFocus %~ replaceLast' parFoc'
        & viewTree . atPath pathToParent . viewFocus .~ parFoc'

-- TODO : This duplicates the code for DirUp.
-- Instead, factor out the computation of newFocus,
-- as a function of parent and an adjustment function.
moveFocus DirDown st = do
  let topView = st ^. viewTree
      path = st ^. pathToFocus
  _ <- pathInBounds topView path
  let pathToParent = take (length path - 1) path
      Just parent = -- safe b/c path is in bounds
        topView ^? atPath pathToParent
      parFoc = parent ^. viewFocus
  _ <- if inBounds (parent ^. viewSubviews) parFoc then Right ()
       else Left $ "Bad focus in " ++ show parent
            ++ " at " ++ show pathToParent
  let parFoc' = min (parFoc + 1)
                $ V.length (parent ^. viewSubviews) - 1
  Right $ st
        & pathToFocus %~ replaceLast' parFoc'
        & viewTree . atPath pathToParent . viewFocus .~ parFoc'


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
insertHosts st = prefixLeft "insertHosts'" $ do
  let (top :: ViewTree) = st ^. viewTree
      (p :: Path) = st ^. pathToFocus
      (r :: Rslt) = st ^. appRslt
  _ <- pathInBounds top p
  let (ma :: Maybe Addr) = top ^?
        atPath p . viewContent . _VResult . viewResultAddr
  a <- let err = "target View must have an Addr"
       in maybe (Left err) Right ma
  (hostRelGroups :: [(CenterRoleView, [Addr])]) <- groupHostRels r a
  (newTrees :: [ViewTree]) <- ifLefts ""
    $ map (hostRelGroup_to_view r) hostRelGroups
  let insert :: ViewTree -> ViewTree
      insert vt = vt & viewSubviews .~
                  V.fromList (foldr (:) preexist newTrees) where
        (preexist :: [ViewTree]) =  V.toList $ vt ^. viewSubviews
  Right $ st & viewTree . atPath p %~ insert


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
