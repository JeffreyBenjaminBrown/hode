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
    pathInBounds -- VTree RsltView -> Path  -> Either String ()
  , atPath       -- Path -> Traversal' (VTree RsltView) (VTree RsltView)
  , moveFocus  -- Direction -> St -> Either String St
  , members_atFocus         -- St -> Either String (ViewMembers, [Addr])
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

import           Control.Lens.Combinators (from)
import           Data.Vector.Lens (vector)
import           Lens.Micro hiding (has)

import Rslt.RLookup
import Rslt.RTypes
import UI.ITypes
import UI.IUtil
import Util.Misc


pathInBounds :: VTree RsltView -> Path -> Either String ()
pathInBounds _ [] = Right ()
pathInBounds vt (p:ps) = let vs = vt ^. vTrees
  in case inBounds vs p of
  True -> pathInBounds (vs V.! p) ps
  False -> Left $ "pathInBounds: " ++ show p ++ "isn't."


atPath :: Path -> Traversal' (VTree RsltView) (VTree RsltView)
atPath [] = lens id $ flip const -- the trivial lens
atPath (p:ps) = vTrees . from vector
                . ix p . atPath ps


moveFocus :: Direction -> St -> Either String St
moveFocus d = prefixLeft "moveFocus"
              . eitherIntoLens buffer (_moveFocus d)

_moveFocus :: Direction -> Buffer -> Either String Buffer
_moveFocus DirLeft b@( _bufferPath -> [] ) = Right b
_moveFocus DirLeft b = Right $ b & bufferPath
                      %~ reverse . tail . reverse

_moveFocus DirRight b = do
  let p = b ^. bufferPath
  foc <- let err = "bad focus " ++ show p
         in maybe (Left err) Right
            $ (b ^. bufferView) ^? atPath p
  if null $ foc ^. vTrees
    then Right b
    else Right $ b & bufferPath %~ (++ [foc ^. vTreeFocus])

_moveFocus DirUp b = do
  let topView = b ^. bufferView
      path = b ^. bufferPath
  _ <- pathInBounds topView path
  let pathToParent = take (length path - 1) path
      Just parent = -- safe b/c path is in bounds
        topView ^? atPath pathToParent
      parFoc = parent ^. vTreeFocus
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in " ++ show parent
            ++ " at " ++ show pathToParent
  let parFoc' = max 0 $ parFoc - 1
  Right $ b
        & bufferPath %~ replaceLast' parFoc'
        & bufferView . atPath pathToParent . vTreeFocus .~ parFoc'

-- TODO : This duplicates the code for DirUp.
-- Instead, factor out the computation of newFocus,
-- as a function of parent and an adjustment function.
_moveFocus DirDown b = do
  let topView = b ^. bufferView
      path = b ^. bufferPath
  _ <- pathInBounds topView path
  let pathToParent = take (length path - 1) path
      Just parent = -- safe b/c path is in bounds
        topView ^? atPath pathToParent
      parFoc = parent ^. vTreeFocus
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in " ++ show parent
            ++ " at " ++ show pathToParent
  let parFoc' = min (parFoc + 1)
                $ V.length (parent ^. vTrees) - 1
  Right $ b
        & bufferPath %~ replaceLast' parFoc'
        & bufferView . atPath pathToParent . vTreeFocus .~ parFoc'


members_atFocus :: St -> Either String (ViewMembers, [Addr])
members_atFocus st = prefixLeft "members_atFocus" $ do
  let (p :: Path) = st ^. buffer . bufferPath
  (foc :: VTree RsltView) <- let left = Left $ "bad path: " ++ show p
    in maybe left Right $ st ^? buffer . bufferView . atPath p
  (a :: Addr) <- case foc ^. vTreeLabel of
    VResult rv -> Right $ rv ^. viewResultAddr
    _ -> Left $ "can only be called from a RsltView with an Addr."
  (as :: [Addr]) <- M.elems <$> has (st ^. appRslt) a
  Right ( ViewMembers a, as )


insertMembers_atFocus :: St -> Either String St
insertMembers_atFocus st = prefixLeft "insertMembers_atFocus" $ do
  ((ms,as) :: (ViewMembers, [Addr])) <- members_atFocus st
  let (topOfNew :: VTree RsltView) = viewLeaf $ VMembers ms
  (leavesOfNew :: [VTree RsltView]) <- map (viewLeaf . VResult)
    <$> ifLefts "" (map (resultView (st ^. appRslt)) as)
  let (new :: VTree RsltView) =
        topOfNew & vTrees .~ V.fromList leavesOfNew
      l = buffer . bufferView . atPath (st ^. buffer . bufferPath) . vTrees
    in Right $ st & l %~ V.cons new


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
  let (top :: VTree RsltView) = st ^. buffer . bufferView
      (p :: Path)             = st ^. buffer . bufferPath
  _ <- pathInBounds top p
  let (ma :: Maybe Addr) = top ^?
        atPath p . vTreeLabel . _VResult . viewResultAddr
  a <- let err = "target RsltView must have an Addr"
       in maybe (Left err) Right ma
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
  Right $ st & buffer . bufferView . atPath (st ^. buffer . bufferPath) %~ insert


hostRelGroup_to_view :: Rslt -> (ViewCenterRole, [Addr])
                     -> Either String (VTree RsltView)
hostRelGroup_to_view r (crv, as) = do
  (rs :: [ViewResult]) <- ifLefts "hostRelGroup_to_view"
    $ map (resultView r) as
  Right $ VTree { _vTreeFocus = 0
                , _vTreeIsFocused = False
                , _vTreeLabel = VCenterRole crv
                , _vTrees =
                    V.fromList $ map (viewLeaf . VResult) rs }


closeSubviews_atFocus :: St -> Either String St
closeSubviews_atFocus = prefixLeft "moveFocus"
                        . eitherIntoLens buffer _closeSubviews_atFocus

_closeSubviews_atFocus :: Buffer -> Either String Buffer
_closeSubviews_atFocus b = prefixLeft "closeSubviews_atFocus" $ do
  _ <- pathInBounds (b ^. bufferView) (b ^. bufferPath)
  _ <- let err = "Closing the root of a view would be silly."
       in if b ^. bufferPath == [] then Left err else Right ()
  let close :: VTree RsltView -> VTree RsltView
      close = vTrees .~ mempty
  Right $ b & bufferView . atPath (b ^. bufferPath) %~ close
