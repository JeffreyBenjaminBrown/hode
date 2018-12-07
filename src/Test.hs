module Test.Main where

import FiniteMap
import SetRBT
import RedBlackTree

import Rslt
import Index
import Index.Positions
import Index.ImgLookup
import Test.Data
import Util


tests :: [Bool]
tests = [ testCheckDb
        , testImgLookup
        , testHoldsPosition
        , testVariety
        , testInvertPositions
        ]

testCheckDb :: Bool
testCheckDb = and
  [ (fmToList $ relsWithoutMatchingTplts testBadFiles testIndex) == [(1001, Rel [1,2] 5), (1002, Rel [1, 2] $ -1000)]
 , (fmToList $ collectionsWithAbsentAddrs testBadFiles testIndex) == [(1002, [-1000])]
  ]

testImgLookup :: Bool
testImgLookup = and
  [ (imgLookup testFiles $ ImgOfAddr 0)               == Just 0
  , (imgLookup testFiles $ ImgOfAddr $ -10000)        == Nothing
  , (imgLookup testFiles $ ImgOfExpr $ Word "needs")     == Just 3
  , (imgLookup testFiles $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  , (imgLookup testFiles $ ImgOfTplt [ImgOfAddr 0, ImgOfExpr $ Word "needs", ImgOfExpr $ Word ""]) == Just 4

  , (imgLookup testFiles $ ImgOfRel [ImgOfAddr 1, ImgOfExpr $ Word "oxygen"] $ ImgOfAddr 4) == Just 5
  , (imgLookup testFiles $ ImgOfRel [ImgOfAddr 1, ImgOfExpr $ Word "oxygen"] $ ImgOfAddr 6) == Nothing
  ]

testHoldsPosition :: Bool
testHoldsPosition = and
  [ holdsPosition testIndex (RoleMember 1, 4) == Just 0
  , holdsPosition testIndex (RoleMember 2, 4) == Just 3
  , holdsPosition testIndex (RoleMember 2, 5) == Just 2
  , holdsPosition testIndex (RoleMember 1, 5) == Just 1
  , holdsPosition testIndex (RoleTplt, 5) == Just 4
  , holdsPosition testIndex (RoleTplt, 6) == Nothing
  ]

testVariety :: Bool
testVariety = and [ variety testIndex 3 == Just (Word',0)
                  , variety testIndex 4 == Just (Tplt',2)
                  , variety testIndex 5 == Just (Rel',2)
                  , variety testIndex 6 == Just (Par',1)
                  , variety testIndex (-133) == Nothing
                  ]

testInvertPositions :: Bool
testInvertPositions =
  let ips = positionsHeldByAll [ (1, [ (RoleMember 1, 11)
                                  , (RoleMember 2, 22) ] )
                            , (11, [ (RoleMember 1, 1)
                                   , (RoleMember 2, 22) ] )
                            , (3, [ (RoleMember 1, 1) ] )
                            ]
      ips' = fmToList $ mapFM (\_ v -> sort (<) $ setRBT2list v) ips
  in ips' == [(1,[(RoleMember 1,3)
                 ,(RoleMember 1,11)])
             ,(11,[(RoleMember 1,1)])
             ,(22,[(RoleMember 2,1)
                  ,(RoleMember 2,11)])]

-- TODO I can call `broken` fron the REPL,
-- but I can't evaluate the RHS of its definition there.
broken :: SetRBT Int
broken = insertRBT 1 $ emptySetRBT (<)
