{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hode.Util.VTree (
    Path
  , Vath
  , VTree(..), vTreeLabel, vTrees, vTreeFocalChild, vTreeIsFocused
  , Vorest
  , VTreeF(..), vTreeLabelF, vTreesF, vTreeFocalChildF, vTreeIsFocusedF
  , vTreeLeaf      -- ^ a -> VTree a
  , vorestLeaf     -- ^ a -> Vorest a
  , pathInBounds   -- ^ VTree a  -> Path -> Either String ()
  , vathInBounds   -- ^ Vorest a -> Vath -> Either String ()
  , atPath         -- ^ Path -> Traversal' (VTree a) (VTree a)
  , atVath         -- ^ Vath -> Traversal' (Vorest a) (VTree a)
  , consUnderFocus -- ^ Path -> VTree a -> VTree a -> Either String (VTree a)
  , moveFocusInTree   -- ^ Direction -> (Path, VTree a)
                    -- -> Either String (Path, VTree a)
  , moveFocusInVorest -- ^ Direction -> (Vath, Vorest a)
                    -- -> Either String (Vath, Vorest a)
  ) where

import           Control.Lens.Combinators (from)
import           Control.Lens.TH
import           Data.Functor.Foldable.TH
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Lens
import           Lens.Micro

import Hode.Util.Misc
import Hode.Util.Direction


-- | A `Path` through a `VTree`. The head of the list is where
-- to go from the `VTree`'s root to its next layer.
type Path = [Int]

-- | A `Vath` is a path through a nonempty `Vorest`.
type Vath = (Int, Path)

-- | A vector-based tree, for viewing things like Views, Buffers, ...
data VTree a = VTree {
    _vTreeLabel :: a
  , _vTrees :: Vorest a
  , _vTreeFocalChild :: Int -- ^ meaningless if `viewSubviews` empty
  , _vTreeIsFocused :: Bool } deriving (Eq, Show, Ord, Functor)
type Vorest a = Vector (VTree a)

makeLenses      ''VTree
makeBaseFunctor ''VTree
makeLenses      ''VTreeF


vTreeLeaf :: a -> VTree a
vTreeLeaf a = VTree { _vTreeLabel = a
                    , _vTrees = V.empty
                    , _vTreeFocalChild = 0
                    , _vTreeIsFocused = False }

vorestLeaf :: a -> Vorest a
vorestLeaf = V.singleton . vTreeLeaf

pathInBounds :: VTree a -> Path -> Either String ()
pathInBounds _ [] = Right ()
pathInBounds vt (p:ps) = let vs = vt ^. vTrees
  in case inBounds vs p of
  True -> pathInBounds (vs V.! p) ps
  False -> Left $ "pathInBounds: " ++ show p ++ "isn't."

vathInBounds :: Vorest a -> Vath -> Either String ()
vathInBounds vor (i,p) = do
  prefixLeft "vathInBounds" $ inBounds' vor i
  pathInBounds (vor V.! i) p

atPath :: Path -> Traversal' (VTree a) (VTree a)
atPath [] = lens id $ flip const -- the trivial lens
atPath (p:ps) = vTrees . from vector
                . ix p . atPath ps

atVath :: Vath -> Traversal' (Vorest a) (VTree a)
atVath (i,p) = from vector . ix i . atPath p

consUnderFocus :: Path -> VTree a -> VTree a -> Either String (VTree a)
consUnderFocus p new host = prefixLeft "consAtFocus" $ do
  _ <- pathInBounds host p
  Right $ host & atPath p . vTrees %~ V.cons new

moveFocusInTree :: Direction -> (Path, VTree a)
          -> Either String (Path, VTree a)
moveFocusInTree DirUp p@([],_) = Right p
moveFocusInTree DirUp (p,a)    = Right (f p, a)
  where f = reverse . tail . reverse
moveFocusInTree DirDown (p,a) = prefixLeft "moveFocusInTree" $ do
  foc <- let err = "bad focus " ++ show p
         in maybe (Left err) Right
            $ a ^? atPath p
  if null $ foc ^. vTrees
    then Right (p                       , a)
    else Right (p ++ [foc ^. vTreeFocalChild], a)

moveFocusInTree DirPrev (p,a) = do
  _ <- pathInBounds a p
  let pathToParent = take (length p - 1) p
      Just parent = -- safe b/c p is in bounds
        a ^? atPath pathToParent
      parFoc = parent ^. vTreeFocalChild
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in parent."
  let parFoc' = max 0 $ parFoc - 1
  Right ( p & replaceLast' parFoc'
        , a & atPath pathToParent . vTreeFocalChild .~ parFoc' )

-- TODO : This duplicates the code for DirPrev.
-- Better: factor out the computation of newFocus,
-- as a function of parent and an adjustment function.
moveFocusInTree DirNext (p,a) = do
  _ <- pathInBounds a p
  let pathToParent = take (length p - 1) p
      Just parent = -- safe b/c p is in bounds
        a ^? atPath pathToParent
      parFoc = parent ^. vTreeFocalChild
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in parent."
  let parFoc' = min (parFoc + 1)
                $ V.length (parent ^. vTrees) - 1
  Right ( p & replaceLast' parFoc'
        , a & atPath pathToParent . vTreeFocalChild .~ parFoc' )

moveFocusInVorest :: forall a. Direction -> (Vath, Vorest a)
                           -> Either String (Vath, Vorest a)
moveFocusInVorest DirUp ((i,[]),vor) = Right ((i,[]),vor)
moveFocusInVorest DirDown ((i,[]),vor) = prefixLeft "moveFocusInVorest" $ do
  inBounds' vor i
  let j = (vor V.! i) ^. vTreeFocalChild
  Right ((i,[j]),vor)
moveFocusInVorest DirPrev ((i,[]),vor) = Right ((i',[]),vor) where
  i' = max (i-1) 0
moveFocusInVorest DirNext ((i,[]),vor) = Right ((i',[]),vor) where
  i' = min (i+1) $ V.length vor - 1
moveFocusInVorest d ((i,p),vor) = prefixLeft "moveFocusInVorest" $ do
  -- TODO : simplify via liftEitherIntoTraversal
  -- (where the traversal is something like `from vector . ix i`)
  inBounds' vor i
  let (t :: VTree a) = vor V.! i
  (p' :: Path, t' :: VTree a) <- moveFocusInTree d (p,t)
  vor' <- let msg = "Impossible: i was already checked by inBounds'."
    in maybe (Left msg) Right $ modifyAt i (const t') vor
  Right ((i,p'), vor')
