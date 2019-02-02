module Test.Rslt.RData where

import qualified Data.Map as M

import Data.Rslt.Index
import Data.Rslt.RTypes


exprs :: RefExprs
exprs = M.fromList
  [ (0, Word "")
  , (1, Word "dog")
  , (2, Word "oxygen")
  , (3, Word "needs")
  , (4, Tplt [0,3,0])
  , (5, Rel [1,2] 4)
  , (6, Par [("The first relationship in this graph is ", 5)] ".")
  ]

badRefExprs :: RefExprs
badRefExprs = M.union exprs newData where
  newData = M.fromList [ (1001, Rel [1,2] 5)
                       , (1002, Rel [1,2] (-1000))
                       ]

rslt = mkRslt exprs
