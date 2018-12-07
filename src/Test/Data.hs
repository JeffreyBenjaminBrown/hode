module Test.Data where

import FiniteMap

import Rslt
import Index


testFiles :: Files
testFiles = listToFM (<)
  [ (0, Word "")
  , (1, Word "dog")
  , (2, Word "oxygen")
  , (3, Word "needs")
  , (4, Tplt [0,3,0])
  , (5, Rel [1,2] 4)
  , (6, Par [("The first relationship in this graph is ", 5)] ".")
  ]

testBadFiles :: Files
testBadFiles = foldl (\fm (k,v) -> addToFM fm k v)  testFiles newData where
  newData = [ (1001, Rel [1,2] 5)
            , (1002, Rel [1,2] (-1000))
            ]

testIndex :: Index
testIndex = index testFiles
