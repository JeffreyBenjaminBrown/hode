{-# LANGUAGE RankNTypes
, ScopedTypeVariables
#-}

module Hode.Util.Misc (
    ShowBrief, showBrief

  -- | = the `Aged` type
  ,  Aged(..), unAged
  , catNews -- ^ [Aged a] -> [a]

  -- | = Fix
  , unFix -- ^ Fix f -> f (Fix f)
  , fmapFirst -- ^ (Bifunctor f, Functor (f b))
              -- => (b -> c) -> Fix (f b) -> Fix (f c)

  -- | = Lenses etc.
  , eitherIntoTraversal -- ^ Traversal' a b -> (b -> Either String b)
                                          -- -> a -> Either String a
  , eitherIntoLens -- ^ Lens' St a -> (a -> Either String a)
                                -- -> St -> Either String St

   -- | = collections
  , intersections      -- ^ Set (Set a) -> Set a
  , replaceNth         -- ^ a -> Int -> [a] -> Either String [a]
  , replaceLast        -- ^ a -> [a] -> Either String [a]
  , replaceLast'       -- ^ a -> [a] -> [a]
  , setFromSetOfMaybes -- ^ Set (Maybe a) -> Set a
  , inBounds           -- ^ Vector a -> Int -> Bool
  , inBounds'          -- ^ Vector a -> Int -> Bool
  , modifyAt           -- ^ Int -> (a -> a) -> Vector a
                       --   -> Maybe (Vector a)
  , transpose          -- ^ [[a]] -> [[a]]
  , beforeDuringAfter  -- ^ (a -> Bool) -> [a] -> Either String ([a],[a],[a])

  -- | = errors
  , keyErr          -- ^ String -> k -> Map k a -> String
  , prefixLeft      -- ^ String -> Either String a -> Either String a
  , ifNothings      -- ^ [Maybe a] -> Maybe [a]
  , LeftStrings
  , ifLefts     -- ^ t (Either String a) -> Either String (t a)
  , ifLefts_set -- ^ Ord a => Set (Either String a) -> Either String (Set a)
  , ifLefts_map -- ^ Ord a =>Map k (Either String a) ->Either String (Map k a)
  , ifLefts_mapKeys -- ^ Map (Either String k) a -> Either String (Map k a)
  ) where


import           Data.Either hiding (lefts)
import           Data.Bifunctor
import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.List   as L
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Lens.Micro


class ShowBrief a where
  showBrief :: a -> String


-- | The `Aged` type

data Aged a = New a | Old a deriving (Eq, Ord, Show)

unAged :: Aged a -> a
unAged (New a) = a
unAged (Old a) = a

catNews :: [Aged a] -> [a]
catNews = catMaybes . map f where
  f (New a) = Just a
  f (Old _) = Nothing


-- | = Fix

unFix :: Fix f -> f (Fix f)
unFix (Fix f) = f

-- | `fmapFirst` can be used, e.g.,
-- to recursively map across the first element of each
-- `ExprFWith` in a `Fix (ExprFWith _)`.
fmapFirst :: (Bifunctor f, Functor (f b))
          => (b -> c) -> Fix (f b)
                      -> Fix (f c)
fmapFirst f (Fix x) = Fix $
  first f $ fmap (fmapFirst f) $ x


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


-- | = Collections

transpose :: forall a. [[a]] -> [[a]]
transpose [] = []
transpose lla0 = let
  lla1 = filter (not . null) lla0
  in if null lla1 then []
     else map head lla1 :
          transpose (map tail lla1)

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

inBounds' :: Vector a -> Int -> Either String ()
inBounds' v i = if inBounds v i then Right ()
  else Left $ "Vector index " ++ show i ++ "out of bounds."

modifyAt :: Int -> (a -> a) -> Vector a -> Maybe (Vector a)
modifyAt i f v
  | not $ inBounds v i = Nothing
  | otherwise = Just ( before
                       V.++ V.singleton (f $ v V.! i)
                       V.++ after )
    where before = V.take i v
          after = V.reverse $ V.take remaining $ V.reverse v
            where remaining = (V.length v - 1) - i

beforeDuringAfter :: (a -> Bool) -> [a] -> Either String ([a],[a],[a])
beforeDuringAfter predi as =
  prefixLeft "beforeDuringAfter: " $ let
  (before, duringAndAfter) = L.span (not . predi) as
  (during, after)          = L.span        predi  duringAndAfter
  in if null $ filter predi after
     then Right (before, during, after)
     else Left $ "More than one contiguous sublist satisfies predicate."


-- | = errors

keyErr :: (Show a, Show k) => String -> k -> Map k a -> String
keyErr callingFunction key m =  callingFunction ++ ": key "
  ++ show key ++ " not found in map " ++ show m ++ ".\n"

prefixLeft :: String -> Either String a -> Either String a
prefixLeft prefix =
  either (\s -> Left $ prefix ++ " " ++ s) Right

ifNothings :: [Maybe a] -> Maybe [a]
ifNothings ms =
  case null $ filter isNothing ms of
    True -> Just $ map fromJust ms
    False -> Nothing

-- | The `Either` monad aborts at the first `Left`,
-- and only reports that one. `LeftStrings` reports them all.
-- To use it, replace constructs like
--   as :: [a] <- mapM f xs
-- with
--   as :: [a] <- ifLefts $ map f xs
class LeftStrings t where
  ifLefts :: t (Either String a) -> Either String (t a)

instance LeftStrings [] where
  ifLefts es =
    let lefts = filter isLeft es
    in case null lefts of
      True -> Right $          map (fromRight $ error "ifLefts: impossible.") es
      False -> Left $ concat $ map (fromLeft  $ error "ifLefts: impossible.")  lefts

ifLefts_set :: Ord a => Set (Either String a) -> Either String (Set a)
ifLefts_set es =
  let lefts = S.filter isLeft es in
  case null lefts of
    True -> Right $ S.map         (fromRight $ error "ifLefts: impossible") es
    False -> Left $ concat (S.map (fromLeft  $ error "ifLefts: impossible") lefts)

ifLefts_map :: Ord a => Map k (Either String a) -> Either String (Map k a)
ifLefts_map m =
  let lefts = filter isLeft $ M.elems m in
  case null lefts of
    True -> Right $ M.map        (fromRight $ error "ifLefts: impossible") m
    False -> Left $ concat $ map (fromLeft  $ error "ifLefts: impossible") lefts

ifLefts_mapKeys :: Ord k
  => Map (Either String k) a -> Either String (Map k a)
ifLefts_mapKeys m = let
  lefts = S.filter isLeft $ M.keysSet m
  impossible = error "ifLefts_mapKeys: impossible."
  in case null lefts of
       True -> Right $ M.mapKeys      (fromRight impossible) m
       False -> Left $ concat $ S.map (fromLeft  impossible) lefts
