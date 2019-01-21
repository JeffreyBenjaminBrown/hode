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

hopeNoLefts :: String -> [Either String a] -> Either String [a]
hopeNoLefts msg es = let
  lefts = filter isLeft es
  impossible = error "hopeNoLefts: impossible."
  in case null lefts of
       True -> Right $ map (fromRight impossible) es
       False -> Left $ msg ++ ": "
         ++ concat (map (fromLeft impossible) lefts)

hopeNoLefts_set :: Ord a
  => String -> Set (Either String a) -> Either String (Set a)
hopeNoLefts_set msg es = let
  lefts = S.filter isLeft es
  impossible = error "hopeNoLefts_set: impossible."
  in case null lefts of
       True -> Right $ S.map (fromRight impossible) es
       False -> Left $ msg ++ ": "
         ++ concat (S.map (fromLeft impossible) lefts)

hopeNoLefts_mapKeys :: Ord k
  => String -> Map (Either String k) a -> Either String (Map k a)
hopeNoLefts_mapKeys msg m = let
  lefts = S.filter isLeft $ M.keysSet m
  impossible = error "hopeNoLefts_mapKeys: impossible."
  in case null lefts of
       True -> Right $ M.mapKeys (fromRight impossible) m
       False -> Left $ msg ++ ": "
         ++ concat (S.map (fromLeft impossible) lefts)


-- | = functions that use the Types module

queryDets :: Query e sp -> Set Var
queryDets (QFind f)  = findDets f
queryDets (QTest t)  = testDets t
queryDets (QVTest t) = varTestDets t
queryDets _          = S.empty

sourceDets :: Source -> Set Var
sourceDets (Source v) = S.singleton v
sourceDets (Source' v dets) = dets

sourceRefs :: Source -> Set Var
sourceRefs (Source v) = S.singleton v
sourceRefs (Source' v dets) = S.insert v dets
