{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S


keyErr :: (Show a, Show k) => String -> k -> Map k a -> String
keyErr callingFunction key map =  callingFunction ++ ": key "
  ++ show key ++ "not found in map " ++ show map ++ ".\n"

isSubsetOfMap :: forall k b. (Ord k, Eq b)
              => Map k b -> Map k b -> Bool
isSubsetOfMap small big = M.foldrWithKey f True small where
  f _ _ False = False -- short-circuit (hence foldr)
  f k b True = M.lookup k big == Just b
