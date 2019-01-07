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
isSubsetOfMap small big = M.foldlWithKey f True small where
  f False _ _ = False -- short-circuit (kind of)
  f True  k b = M.lookup k big == Just b
