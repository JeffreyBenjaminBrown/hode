{-# LANGUAGE ScopedTypeVariables #-}

module Space.Graph.GQuery where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Query.MkLeaf
import Space.Graph
import Types
import Util


-- | == for building `Query`s

findChildren, findParents :: (Ord e, Show e)
                          => Either e Var -> Find e (Graph e)
findChildren = mkFindFrom "findChildren" children
findParents  = mkFindFrom "findParents"  parents
