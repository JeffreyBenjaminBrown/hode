{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.GQuery where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Search.Query.MkLeaf
import Data.Graph
import Search.Types
import Util


-- | == for building `Query`s

findChildren, findParents :: (Ord e, Show e)
                          => Either e Var -> Find e (Graph e)
findChildren = findFrom "findChildren" children
findParents  = findFrom "findParents"  parents
