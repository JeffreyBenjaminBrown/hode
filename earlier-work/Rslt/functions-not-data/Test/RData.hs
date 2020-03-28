module Test.Rslt.RData where

import qualified Data.Map as M

import Space.Rslt.Types
import Space.Rslt.Index


exprs :: Exprs
exprs = M.fromList
  [ (0, Word "")
  , (1, Word "dog")
  , (2, Word "oxygen")
  , (3, Word "needs")
  , (4, Tplt [0,3,0])
  , (5, Rel [1,2] 4)
  , (6, Par [("The first relationship in this graph is ", 5)] ".")
  ]

badExprs :: Exprs
badExprs = M.union exprs newData where
  newData = M.fromList [ (1001, Rel [1,2] 5)
                       , (1002, Rel [1,2] (-1000))
                       ]

index :: Index
index = mkIndex exprs

rslt :: Rslt
rslt = (exprs, index)
