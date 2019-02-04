module Test.Rslt.RData where

import qualified Data.Map as M

import Rslt.Index
import Rslt.RTypes


-- | = A small Rslt

refExprs :: RefExprs
refExprs = M.fromList
  [ (0, Word' "")
  , (1, Word' "dog")
  , (2, Word' "oxygen")
  , (3, Word' "needs")
  , (4, Tplt' [0,3,0])
  , (5, Rel' [1,2] 4)
  , (6, Par' [("The first relationship in this graph is ", 5)] ".")
  ]

badRefExprs :: RefExprs
badRefExprs = M.union refExprs newData where
  newData = M.fromList [ (1001, Rel' [1,2] 5)
                       , (1002, Rel' [1,2] (-1000))
                       ]

rslt = mkRslt refExprs


-- | A bigger one

big :: Rslt
big = mkRslt $ M.fromList
  [ (  0, Word' "0")
  , (  1, Word' "1")
  , (  2, Word' "2")
  , (  3, Word' "3")
  , (  4, Tplt' [0,0])   -- the unary rel
  , (  5, Tplt' [0,0,0]) -- the binary rel
  , (  6, Rel' [1]   4)
  , (  7, Rel' [1,2] 5)
  , (  8, Rel' [6,7] 5)
  , (  9, Rel' [2,3] 5)
  , ( 10, Rel' [7,8] 5)
  ]
