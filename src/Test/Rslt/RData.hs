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


-- | A big, abstract one

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


-- | = A bigger, concrete one

b2 :: Rslt
b2 = mkRslt $ M.fromList
  [ ( 1, Word' "")
  , ( 2, Word' "fish")
  , ( 3, Word' "water")
  , ( 4, Word' "need")
  , ( 5, Word' "like")
  , ( 6, Word' "jumping")
  , ( 7, Tplt' [0,4,0] ) -- _ need _
  , ( 8, Tplt' [0,5,0] ) -- _ like _
  , ( 9, Rel' [2,3] 4) -- fish #need water
  , (10, Rel' [2,6] 5) -- fish #like jumping
  , (11, Word' "horses")
  , (12, Rel' [11,6] 5) -- horses #like jumping
  , (13, Word' "exercise")
  , (14, Word' "is")
  , (15, Tplt' [0,14,0]) -- _ is _
  , (16, Rel' [6,13] 15 ) -- jumping #is exercise
  ]
