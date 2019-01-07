{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ExistentialQuantification #-}

module Util where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S


isSubsetOfMap :: forall k b. (Ord k, Eq b)
              => Map k b -> Map k b -> Bool
isSubsetOfMap small big = M.foldrWithKey f True small where
  f _ _ False = False -- short-circuit (hence foldr)
  f k b True = M.lookup k big == Just b
