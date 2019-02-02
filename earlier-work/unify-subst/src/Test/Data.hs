{-# LANGUAGE TupleSections #-}

module Test.Data where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Graph
import Types


-- | Numbers in [0,9] are first generation, [10,19] second, etc.
-- There's one cycle, between 2 and 13.
g :: Graph
g = M.fromList [ ( 1, S.fromList [11, 12])
               , ( 2, S.fromList [12, 13])
               , (11, S.fromList [21, 22])
               , (12, S.fromList [22, 23])
               , (13, S.fromList [2     ])
               , (21, S.empty)
               , (22, S.empty)
               , (23, S.empty)
               ]

-- | I'm not sure I even need edges to meaningfully test this code.
disconnected100 :: Graph
disconnected100 = M.fromList $ map (, S.empty) [1..100]
