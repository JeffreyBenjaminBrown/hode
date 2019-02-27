module Graph where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S


type Graph  = Map Int (Set Int)

children :: Graph -> Int -> Set Int
children g i = maybe S.empty id $ M.lookup i g

parents :: Graph -> Int -> Set Int
parents g i = M.keysSet $ M.filter (elem i) g
