{-# LANGUAGE LambdaCase #-}

module Util where

import           Data.Maybe
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Types


setUnions :: Ord a => Set (Set a) -> Set a
setUnions = S.foldl S.union S.empty

setFoldl1 :: Ord a => (Set a -> Set a -> Set a)
                   -> Set (Set a) -> Set a
setFoldl1 f s = S.foldl f head tail
  where head = maybe S.empty id $ S.lookupMin s
        tail = S.delete head s


-- | = Because And [And (x,y), And (z,w) ] = And [x,y,z,w], etc.
-- But I'm not sure I'll ever use them.
partitionQAnds, partitionQOrs :: [Query e sp] -> ([Query e sp], [Query e sp])
partitionQAnds qs = (concat $ map (\(QAnd qs) -> qs) ands, others)
  where (ands, others) = L.partition (\case QAnd _ -> True; _ -> False) qs
partitionQOrs qs = (concat $ map (\(QOr qs) -> qs) ors, others)
  where (ors, others) = L.partition (\case QOr _ -> True; _ -> False) qs
