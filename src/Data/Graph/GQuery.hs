{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.GQuery where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Query.MkLeaf
import Data.Graph
import Types
import Util


-- | == for building `Query`s

findChildren, findParents :: (Ord e, Show e)
                          => Either e Var -> Find e (Graph e)
findChildren = findFrom "findChildren" children
findParents  = findFrom "findParents"  parents
