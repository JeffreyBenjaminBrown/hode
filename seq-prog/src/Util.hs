{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


keyErr :: (Show a, Show k) => String -> k -> Map k a -> String
keyErr callingFunction key map =  callingFunction ++ ": key "
  ++ show key ++ "not found in map " ++ show map ++ ".\n"

isSubsetOfMap :: forall k b. (Ord k, Eq b)
              => Map k b -> Map k b -> Bool
isSubsetOfMap small big = M.foldrWithKey f True small where
  f _ _ False = False -- short-circuit (hence foldr)
  f k b True = M.lookup k big == Just b

setFromSetOfMaybes :: Ord a => Set (Maybe a) -> Set a
setFromSetOfMaybes = S.map fromJust . S.filter (not . isNothing)


-- | = functions that use the Types module

queryDets :: Query e sp -> Set Var
queryDets (QFind f)        = findDets f
queryDets (QTest t)        = testDets t
queryDets (QVarTest t)     = varTestDets t
queryDets _ = S.empty

sourceDets :: Source -> Set Var
sourceDets (Source v) = S.singleton v
sourceDets (Source' v dets) = dets

sourceRefs :: Source -> Set Var
sourceRefs (Source v) = S.singleton v
sourceRefs (Source' v dets) = S.insert v dets
