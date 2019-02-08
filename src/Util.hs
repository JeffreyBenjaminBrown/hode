{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Data.Either
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S


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

keyErr :: (Show a, Show k) => String -> k -> Map k a -> String
keyErr callingFunction key map =  callingFunction ++ ": key "
  ++ show key ++ " not found in map " ++ show map ++ ".\n"

setFromSetOfMaybes :: Ord a => Set (Maybe a) -> Set a
setFromSetOfMaybes = S.map fromJust . S.filter (not . isNothing)

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
