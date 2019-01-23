{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Data.Either
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


keyErr :: (Show a, Show k) => String -> k -> Map k a -> String
keyErr callingFunction key map =  callingFunction ++ ": key "
  ++ show key ++ "not found in map " ++ show map ++ ".\n"

setFromSetOfMaybes :: Ord a => Set (Maybe a) -> Set a
setFromSetOfMaybes = S.map fromJust . S.filter (not . isNothing)

ifLefts :: String -> [Either String a] -> Either String [a]
ifLefts msg es = let
  lefts = filter isLeft es
  impossible = error "ifLefts: impossible."
  in case null lefts of
       True -> Right $ map (fromRight impossible) es
       False -> Left $ msg ++ ": "
         ++ concat (map (fromLeft impossible) lefts)

ifLefts_set :: Ord a
  => String -> Set (Either String a) -> Either String (Set a)
ifLefts_set msg es = let
  lefts = S.filter isLeft es
  impossible = error "ifLefts_set: impossible."
  in case null lefts of
       True -> Right $ S.map (fromRight impossible) es
       False -> Left $ msg ++ ": "
         ++ concat (S.map (fromLeft impossible) lefts)

ifLefts_mapKeys :: Ord k
  => String -> Map (Either String k) a -> Either String (Map k a)
ifLefts_mapKeys msg m = let
  lefts = S.filter isLeft $ M.keysSet m
  impossible = error "ifLefts_mapKeys: impossible."
  in case null lefts of
       True -> Right $ M.mapKeys (fromRight impossible) m
       False -> Left $ msg ++ ": "
         ++ concat (S.map (fromLeft impossible) lefts)
