module Test where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Query
import Subst
import Types
import Util


tests = runTestTT $ TestList
  [ TestLabel "testInvertMapToSet" testInvertMapToSet
  , TestLabel "testFindable" testFindable
  , TestLabel "testValidExistentials" testValidExistentials
  , TestLabel "testReconcile2" testReconcile2
  , TestLabel "testReconcile1toMany" testReconcile1toMany
  , TestLabel "testReconcile2sets" testReconcile2sets
  , TestLabel "testReconcile" testReconcile
  , TestLabel "testVarFuncSubsts" testVarFuncSubsts
  , TestLabel "testRestrictCondVals1" testRestrictCondVals1
  , TestLabel "testVarFuncCondVals" testVarFuncCondVals
  , TestLabel "testIsSubsetOfMap" testIsSubsetOfMap
  ]

testIsSubsetOfMap = TestCase $ do
  let m  = M.fromList [ (1, S.fromList [1,2]), (2, S.fromList [2,3]) ]
      m' = M.fromList [ (1, S.fromList [1,2]), (2, S.fromList [2,4]) ]
      n  = M.fromList [ (1, S.fromList [1,2]) ]
  assertBool "1" $       isSubsetOfMap n m
  assertBool "1" $ not $ isSubsetOfMap m n
  assertBool "1" $       isSubsetOfMap n m'
  assertBool "1" $ not $ isSubsetOfMap m m'

testVarFuncCondVals = TestCase $ do
  let (a,b,c,x) = (Var "a",Var "b",Var "c",Var "x")
      vf_a_bc = VarFunc a (S.fromList [b, c])
      vf_a_b  = VarFunc a (S.fromList [b   ])
      vf_a    = VarFunc a (S.empty)
      s_b1c1 = M.fromList [ (b,1), (c,1) ]
      ra = M.fromList [
        ( a, M.fromList [ (1, S.singleton mempty) ] ) ] :: Result
      r = M.fromList
        [ ( a, M.fromList [ (1, S.singleton mempty)
                          , (2, S.singleton $ M.singleton x 22) ] )
        , ( b, M.fromList [ (1, S.fromList [ M.singleton a 2 ] ) ] )
        ] :: Result

  putStrLn $ "\n\n" ++ show (varFuncCondVals r s_b1c1 vf_a_b) ++ "\n\n"
  assertBool "3" $ varFuncCondVals r s_b1c1 vf_a_b
    == M.fromList [ (2, S.singleton $ M.singleton x 22) ]
  assertBool "2" $ varFuncCondVals ra s_b1c1 vf_a
    == M.fromList [ (1, S.singleton M.empty) ]
  assertBool "1" $ varFuncCondVals ra M.empty vf_a
    == M.fromList [ (1, S.singleton M.empty) ]
--  varFuncCondVals :: Result -> Subst -> VarFunc -> ConditionedElts

testRestrictCondVals1 = TestCase $ do
  let (x,y,z) = (Var "x",Var "y",Var"z")
      subst = M.fromList [ (x,1), (y,11) ]
      ces = M.fromList [ (1, S.fromList [ subst
                                        , M.insert z 111 subst ] )
                       , (2, S.fromList [ M.insert x 2 subst ] ) ]
  putStrLn $ "\n RestrictCondVals \n"
    ++ show (restrictCondVals1 subst ces) ++ "\n\n"
  assertBool "1" $ restrictCondVals1 subst ces
         == M.fromList [ (1, S.fromList [ subst
                                        , M.insert z 111 subst ] ) ]
  -- TODO speed|memory : Notice how the z-binding is kept, whether or not
  -- it is relevant. If z is not used later, then maybe it should not be
  -- kept. This would prevent treating the Subst with the z-binding as
  -- distinct from the Subst without it, hence avoid duplicating some work.

testVarFuncSubsts = TestCase $ do
  let xCondVals = M.fromList -- x could be 1 or 2, if ...
        [ (1, S.fromList [ M.fromList [ (Var "a", 1) ] ] )
        , (2, S.fromList [ M.fromList [ (Var "a", 2)
                                      , (Var "b", 2) ] ] ) ]
      yCondVals = M.fromList -- y could be 3 or 4, if ...
        [ (3, S.fromList [ M.fromList [ (Var "a", 1) ] ] )
        , (4, S.fromList [ M.fromList [ (Var "b", 2)
                                      , (Var "c", 2) ]
                         , M.fromList [ (Var "b", 3)
                                      , (Var "c", 3) ] ] ) ]
      r = M.fromList [ ((Var "x"), xCondVals)
                     , ((Var "y"), yCondVals) ]
      xySubst xVal yVal = M.fromList [ ((Var "x"), xVal)
                                   , ((Var "y"), yVal) ]
      a_x  = VarFunc (Var "a") (S.fromList [(Var "x")           ])
      a_xy = VarFunc (Var "a") (S.fromList [(Var "x"), (Var "y")])
  assertBool "0" $ varFuncSubsts r (xySubst 1 4) a_x
    == S.fromList [ M.fromList [ (Var "a", 1) ] ]
  assertBool "1" $ varFuncSubsts r (xySubst 1 3) a_xy
    == S.fromList [ M.fromList [ (Var "a", 1) ] ]
  assertBool "2" $ varFuncSubsts r (xySubst 2 3) a_xy
    == S.empty
  assertBool "3" $ varFuncSubsts r (xySubst 1 4) a_xy
    == S.fromList [ M.fromList [ (Var "a", 1), (Var "b", 2), (Var "c", 2) ]
                  , M.fromList [ (Var "a", 1), (Var "b", 3), (Var "c", 3) ] ]

testReconcile = TestCase $ do
  let x1    = S.singleton ( M.singleton (Var "x") 1 )
      x1_x2 = S.fromList  [ M.singleton (Var "x") 1
                          , M.singleton (Var "x") 2 ]
      x1_x3 = S.fromList  [ M.singleton (Var "x") 1
                          , M.singleton (Var "x") 3 ]
  assertBool "1" $ reconcile (S.fromList [x1_x2, x1_x3]) == x1

testReconcile2sets = TestCase $ do
  let x1 = M.singleton (Var "x") 1
      y1 = M.singleton (Var "y") 1
      y2 = M.singleton (Var "y") 2
      z2 = M.singleton (Var "z") 2
      x1y2 = M.fromList [ ((Var "x"),1)
                        , ((Var "y"),2) ]
      x1z2 = M.fromList [ ((Var "x"),1)
                        , ((Var "z"),2) ]
      x1y1z2 = M.fromList [ ((Var "x"),1)
                          , ((Var "y"),1)
                          , ((Var "z"),2) ]
      x1y2z2 = M.fromList [ ((Var "x"),1)
                          , ((Var "y"),2)
                          , ((Var "z"),2) ]
      ss = S.singleton
      sf = S.fromList
  assertBool "0" $ reconcile2sets S.empty (sf [x1y2, x1z2])
                               == S.empty
  assertBool "0.1" $ reconcile2sets (ss x1) S.empty
                                         == S.empty
  assertBool "1" $ reconcile2sets (ss x1) (sf [x1y2, x1z2])
                                        == sf [x1y2, x1z2]
  assertBool "2" $ reconcile2sets (sf [x1,z2]) (sf [x1y2, x1z2])
                                             == sf [x1y2, x1z2, x1y2z2]
  assertBool "3" $ reconcile2sets (ss y1) (sf [x1y2, x1z2]) == ss x1y1z2

testReconcile1toMany = TestCase $ do
  let x1 = M.singleton (Var "x") 1
      y1 = M.singleton (Var "y") 1
      y2 = M.singleton (Var "y") 2
      x1y2 = M.fromList [ ((Var "x"),1)
                        , ((Var "y"),2)]
  assertBool "1" $ reconcile1toMany x1y2 (S.fromList [x1, y1, y2] )
                     == S.singleton x1y2
  assertBool "2" $ reconcile1toMany x1y2 (S.singleton M.empty)
                     == S.singleton x1y2
  assertBool "3" $ reconcile1toMany x1y2 S.empty
                     == S.empty

testReconcile2 = TestCase $ do
  let x1 = M.singleton (Var "x") 1
      y1 = M.singleton (Var "y") 1
      y2 = M.singleton (Var "y") 2
      x1y2 = M.fromList [ ((Var "x"),1)
                        , ((Var "y"),2)]
  assertBool "0" $ reconcile2 M.empty M.empty == Just M.empty
  assertBool "1" $ reconcile2 M.empty x1      == Just x1
  assertBool "2" $ reconcile2 x1y2    M.empty == Just x1y2
  assertBool "3" $ reconcile2 x1      y2      == Just x1y2
  assertBool "4" $ reconcile2 y2      x1y2    == Just x1y2
  assertBool "5" $ reconcile2 y1      x1y2    == Nothing

testValidExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = VarFunc (Var "x") $ S.empty
      y   = VarFunc (Var "y") $ S.empty
      qx  = ForSome x qf
      qy  = ForSome y qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ disjointExistentials (ForSome x qx)  == False
  assertBool "2" $ disjointExistentials (ForSome y qx)  == True
  assertBool "3" $ disjointExistentials qxy             == True
  assertBool "4" $ disjointExistentials (QAnd [qx,qxy]) == False

testFindable = TestCase $ do
  let qf = QFind $ Find (\_ _    -> S.empty) S.empty
      qc = QCond $ Cond (\_ _  _ -> False  ) S.empty
  assertBool "1" $ findable (QAnd [qf, qc]) == True
  assertBool "2" $ findable (QAnd [qf]    ) == True
  assertBool "3" $ findable (QAnd [qc]    ) == False
  assertBool "4" $ findable (QAnd []      ) == False
  assertBool "5" $ findable (QOr  [qf, qc]) == False
  assertBool "6" $ findable (QOr  [qf]    ) == True
  assertBool "7" $ findable (QOr  [qc]    ) == False
  assertBool "8" $ findable (QOr  []      ) == False

testInvertMapToSet = TestCase $ do
  let g = M.fromList [ (1, S.fromList [2,3] )
                     , (2, S.fromList [1,3] ) ]
      h = M.fromList [ (1, S.fromList [2  ] )
                     , (2, S.fromList [1  ] )
                     , (3, S.fromList [1,2] ) ]
  assertBool "1" $ invertMapToSet g == h
  assertBool "2" $ invertMapToSet h == g
