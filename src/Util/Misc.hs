{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Util.Misc (
    module U

  -- | = Lenses etc.
  , eitherIntoTraversal -- ^ Traversal' a b -> (b -> Either String b)
                                          -- -> a -> Either String a
  , eitherIntoLens -- ^ Lens' St a -> (a -> Either String a)
                                -- -> St -> Either String St

  , pathInBounds -- ^ VTree RsltView -> Path  -> Either String ()
  , atPath  -- ^ Path -> Traversal' (VTree a)  (VTree a)
  , atVath  -- ^ Vath -> Traversal' (Vorest a) (VTree a)
  , moveFocus' -- ^ Direction -> (Path, VTree a)
             -- -> Either String (Path, VTree a)

  -- | = collections
  ,  intersections      -- ^ Set (Set a) -> Set a
  , replaceNth         -- ^ a -> Int -> [a] -> Either String [a]
  , replaceLast        -- ^ a -> [a] -> Either String [a]
  , replaceLast'       -- ^ a -> [a] -> [a]
  , setFromSetOfMaybes -- ^ Set (Maybe a) -> Set a
  , inBounds           -- ^ Vector a -> Int -> Bool
  , modifyAt           -- ^ Int -> (a -> a) -> Vector a
                       --   -> Maybe (Vector a)

  -- | = errors
  , keyErr          -- ^ String -> k -> Map k a -> String
  , mapLeft         -- ^ (a -> a') -> Either a b -> Either a' b
  , prefixLeft      -- ^ String -> Either String a -> Either String a
  , ifNothings      -- ^ [Maybe a] -> Maybe [a]
  , ifLefts         -- ^ String -> [Either String a] -> Either String [a]
  , ifLefts_set     -- ^ String -> Set (Either String a) -> Either String (Set a)
  , ifLefts_mapKeys -- ^ String -> Map (Either String k) a -> Either String (Map k a)
  , ifLefts_map     -- ^ String -> Map k (Either String a) -> Either String (Map k a)
  ) where

import           Data.Either hiding (lefts)
import           Data.Maybe
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Lens
import           Lens.Micro
import           Control.Lens.Combinators (from)

import           Util.UTypes as U


-- | = Lenses etc.

eitherIntoTraversal ::
  Traversal' a b -> (b -> Either String b) -> a -> Either String a
eitherIntoTraversal l f st = do
  (b :: b) <- let msg = "Traversal' returned Nothing."
    in maybe (Left msg) Right $ st ^? l
  b' <- f b
  Right $ st & l .~ b'

eitherIntoLens :: Lens' a b -> (b -> Either left b) -> a -> Either left a
eitherIntoLens l f st = do b' <- f $ st ^. l
                           Right $ st & l .~ b'

pathInBounds :: VTree a -> Path -> Either String ()
pathInBounds _ [] = Right ()
pathInBounds vt (p:ps) = let vs = vt ^. vTrees
  in case inBounds vs p of
  True -> pathInBounds (vs V.! p) ps
  False -> Left $ "pathInBounds: " ++ show p ++ "isn't."

atPath :: Path -> Traversal' (VTree a) (VTree a)
atPath [] = lens id $ flip const -- the trivial lens
atPath (p:ps) = vTrees . from vector
                . ix p . atPath ps

atVath :: Vath -> Traversal' (Vorest a) (VTree a)
atVath (i,p) = from vector . ix i . atPath p

moveFocus' :: Direction -> (Path, VTree a)
          -> Either String (Path, VTree a)
moveFocus' DirUp p@([],_) = Right p
moveFocus' DirUp (p,a)    = Right (f p, a)
  where f = reverse . tail . reverse
moveFocus' DirDown (p,a) = prefixLeft "moveFocus" $ do
  foc <- let err = "bad focus " ++ show p
         in maybe (Left err) Right
            $ a ^? atPath p
  if null $ foc ^. vTrees
    then Right (p                       , a)
    else Right (p ++ [foc ^. vTreeFocus], a)

moveFocus' DirPrev (p,a) = do
  _ <- pathInBounds a p
  let pathToParent = take (length p - 1) p
      Just parent = -- safe b/c p is in bounds
        a ^? atPath pathToParent
      parFoc = parent ^. vTreeFocus
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in parent."
  let parFoc' = max 0 $ parFoc - 1
  Right ( p & replaceLast' parFoc'
        , a & atPath pathToParent . vTreeFocus .~ parFoc' )

-- TODO : This duplicates the code for DirPrev.
-- Better: factor out the computation of newFocus,
-- as a function of parent and an adjustment function.
moveFocus' DirNext (p,a) = do
  _ <- pathInBounds a p
  let pathToParent = take (length p - 1) p
      Just parent = -- safe b/c p is in bounds
        a ^? atPath pathToParent
      parFoc = parent ^. vTreeFocus
  _ <- if inBounds (parent ^. vTrees) parFoc then Right ()
       else Left $ "Bad focus in parent."
  let parFoc' = min (parFoc + 1)
                $ V.length (parent ^. vTrees) - 1
  Right ( p & replaceLast' parFoc'
        , a & atPath pathToParent . vTreeFocus .~ parFoc' )


-- | = Collections

-- | PITFALL: In math, the intersection of the empty set is the entire
-- universe, just like `and [] == True`. But that's impractical.
intersections :: Ord a => Set (Set a) -> Set a
intersections s | null s    = S.empty
                | otherwise = foldl1 S.intersection $ S.toList s

replaceNth :: a -> Int -> [a] -> Either String [a]
replaceNth a n as = do
  if not $ n < 1 then Right ()
    else Left $ "replaceNth: index " ++ show n ++ " not a natural number.\n"
  if not $ n > length as then Right ()
    else Left $ "replaceNth: index " ++ show n ++ " greater than list length.\n"
  let (before, _:after) = splitAt (n-1) as
  Right $ before ++ a : after

replaceLast :: a -> [a] -> Either String [a]
replaceLast _ [] = Left "replaceLast given an empty list."
replaceLast a as = Right $ reverse $ a : tail (reverse as)

replaceLast' :: a -> [a] -> [a]
replaceLast' _ [] = []
replaceLast' a as = reverse $ a : tail (reverse as)

setFromSetOfMaybes :: Ord a => Set (Maybe a) -> Set a
setFromSetOfMaybes = S.map fromJust . S.filter (not . isNothing)

inBounds :: Vector a -> Int -> Bool
inBounds v i = i >= 0 &&
               i <= V.length v - 1

modifyAt :: Int -> (a -> a) -> Vector a -> Maybe (Vector a)
modifyAt i f v
  | not $ inBounds v i = Nothing
  | otherwise = Just ( before
                       V.++ V.singleton (f $ v V.! i)
                       V.++ after )
    where before = V.take i v
          after = V.reverse $ V.take remaining $ V.reverse v
            where remaining = (V.length v - 1) - i


-- | = errors

keyErr :: (Show a, Show k) => String -> k -> Map k a -> String
keyErr callingFunction key m =  callingFunction ++ ": key "
  ++ show key ++ " not found in map " ++ show m ++ ".\n"

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft _ (Right x) = Right x
mapLeft f (Left x) = Left $ f x

prefixLeft :: String -> Either String a -> Either String a
prefixLeft prefix =
  either (\s -> Left $ prefix ++ " --called-> " ++ s) Right

ifNothings :: [Maybe a] -> Maybe [a]
ifNothings ms = let
  nothings = filter isNothing ms
  in case null nothings of
       True -> Just $ map fromJust ms
       False -> Nothing

ifLefts :: String -> [Either String a] -> Either String [a]
ifLefts msg es = let
  lefts = filter isLeft es
  impossible = error "ifLefts: impossible."
  in case null lefts of
       True -> Right $ map (fromRight impossible) es
       False -> Left $ msg ++ " --called-> "
         ++ concat (map (fromLeft impossible) lefts)

ifLefts_set :: Ord a
  => String -> Set (Either String a) -> Either String (Set a)
ifLefts_set msg es = let
  lefts = S.filter isLeft es
  impossible = error "ifLefts_set: impossible."
  in case null lefts of
       True -> Right $ S.map (fromRight impossible) es
       False -> Left $ msg ++ " --called-> "
         ++ concat (S.map (fromLeft impossible) lefts)

ifLefts_mapKeys :: Ord k
  => String -> Map (Either String k) a -> Either String (Map k a)
ifLefts_mapKeys msg m = let
  lefts = S.filter isLeft $ M.keysSet m
  impossible = error "ifLefts_mapKeys: impossible."
  in case null lefts of
       True -> Right $ M.mapKeys (fromRight impossible) m
       False -> Left $ msg ++ " --called-> "
         ++ concat (S.map (fromLeft impossible) lefts)

ifLefts_map :: Ord k
  => String -> Map k (Either String a) -> Either String (Map k a)
ifLefts_map msg m = let
  lefts = filter isLeft $ M.elems m
  impossible = error "ifLefts_map: impossible."
  in case null lefts of
       True -> Right $ M.map (fromRight impossible) m
       False -> Left $ msg ++ " --called-> "
         ++ concat (map (fromLeft impossible) lefts)

