module Test where

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S

import Rslt
import Index
import Index.Positions
import Index.ImgLookup
import Search
import qualified Test.Data as D
import Util


tests :: [Bool]
tests = [ and testJoin2Substs
        , and testSearch
        , and testCheckDb
        , and testImgLookup
        , and testHoldsPosition
        , and testVariety
        , and testInvertPositions
        ]

testJoin2Substs :: [Bool]
testJoin2Substs = 
  [ join2Substs M.empty M.empty == Right M.empty
  , join2Substs ( M.fromList [(Var "a",0), (Var "b",1)])
                ( M.fromList [(Var "b",1), (Var "a",0)])
      == (Right $ M.fromList [(Var "b",1), (Var "a",0)])
  , join2Substs ( M.fromList [(Var "a",0), (Var "b",1)])
                ( M.fromList [(Var "b",1)] )
      == (Right $ M.fromList [(Var "b",1), (Var "a",0)])
  ]

x = join2Substs ( M.fromList [(Var "a",0), (Var "b",1)])
                ( M.fromList [(Var "b",1)] )

testSearch :: [Bool]
testSearch =
  [ search' (D.index) (QHasInRole (RoleMember 1) $ QImg $ ImgOfExpr $ Word "") == S.fromList [4]
  , search' (D.index) (QHasInRole (RoleMember 3) $ QImg $ ImgOfExpr $ Word "") == S.fromList [4]
  , search' (D.index) (QHasInRole (RoleMember 2) $ QImg $ ImgOfExpr $ Word "") == S.empty
  ]

testCheckDb :: [Bool]
testCheckDb =
  [ (M.toList $ relsWithoutMatchingTplts D.badFiles D.index) == [(1001, Rel [1,2] 5), (1002, Rel [1, 2] $ -1000)]
 , (M.toList $ collectionsWithAbsentAddrs D.badFiles D.index) == [(1002, [-1000])]
  ]

testImgLookup :: [Bool]
testImgLookup =
  [ (imgLookup D.files $ ImgOfAddr 0)              == Just 0
  , (imgLookup D.files $ ImgOfAddr $ -10000)       == Nothing
  , (imgLookup D.files $ ImgOfExpr $ Word "needs") == Just 3
  , (imgLookup D.files $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  , (imgLookup D.files $ ImgOfTplt [ImgOfAddr 0, ImgOfExpr $ Word "needs", ImgOfExpr $ Word ""]) == Just 4

  , (imgLookup D.files $ ImgOfRel [ImgOfAddr 1, ImgOfExpr $ Word "oxygen"] $ ImgOfAddr 4) == Just 5
  , (imgLookup D.files $ ImgOfRel [ImgOfAddr 1, ImgOfExpr $ Word "oxygen"] $ ImgOfAddr 6) == Nothing
  ]

testHoldsPosition :: [Bool]
testHoldsPosition =
  [ holdsPosition D.index (RoleMember 1, 4) == Just 0
  , holdsPosition D.index (RoleMember 2, 4) == Just 3
  , holdsPosition D.index (RoleMember 2, 5) == Just 2
  , holdsPosition D.index (RoleMember 1, 5) == Just 1
  , holdsPosition D.index (RoleTplt, 5) == Just 4
  , holdsPosition D.index (RoleTplt, 6) == Nothing
  ]

testVariety :: [Bool]
testVariety = [ variety D.index 3 == Just (Word',0)
              , variety D.index 4 == Just (Tplt',2)
              , variety D.index 5 == Just (Rel',2)
              , variety D.index 6 == Just (Par',1)
              , variety D.index (-133) == Nothing
              ]

testInvertPositions :: [Bool]
testInvertPositions =
  let ips = positionsHeldByAll [ (1,  [ (RoleMember 1, 11 )
                                      , (RoleMember 2, 22 ) ] )
                               , (11, [ (RoleMember 1, 1  )
                                      , (RoleMember 2, 22 ) ] )
                               , (3,  [ (RoleMember 1, 1  ) ] )
                               ]
  in [ ips == M.fromList [(1,  S.fromList [(RoleMember 1,3  )
                                         ,(RoleMember 1,11 )])
                        ,(11, S.fromList [(RoleMember 1,1  )])
                        ,(22, S.fromList [(RoleMember 2,1  )
                                         ,(RoleMember 2,11 )])]
     ]
