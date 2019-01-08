{-# LANGUAGE ScopedTypeVariables #-}

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
  [ TestLabel "testIsSubsetOfMap" testIsSubsetOfMap
  , TestLabel "testInvertMapToSet" testInvertMapToSet
  , TestLabel "testFindable" testFindable
  , TestLabel "testValidExistentials" testValidExistentials
  , TestLabel "testReconcile2" testReconcile2
  , TestLabel "testReconcile1ToMany" testReconcile1ToMany
  , TestLabel "testReconcile2sets" testReconcile2sets
  , TestLabel "testReconcile" testReconcile
  , TestLabel "testVarFuncSubsts" testVarFuncSubsts
  , TestLabel "testRestrictCondElts1" testRestrictCondElts1
  , TestLabel "testRestrictCondElts" testRestrictCondElts
  , TestLabel "testVarFuncToCondElts" testVarFuncToCondElts
  , TestLabel "testSubstToCondElts" testSubstToCondElts
  , TestLabel "testSetSubstToCondElts" testSetSubstToCondElts
  ]

testSetSubstToCondElts = TestCase $ do
  let (a,b,c,x) = (Var "a",Var "b",Var "c",Var "x")
      s = M.fromList [ (a,1), (b,2) ]
      t = M.fromList [ (a,1), (b,3) ]
      u = M.fromList [ (a,1), (b,3)
                     , (a,2), (b,3), (c,4) ]
  assertBool "1" $ setSubstToCondElts a (S.fromList [s, t,u,M.empty])
    == ( M.fromList
         [ (1, S.fromList [ M.singleton b 2
                          , M.singleton b 3 ] )
         , (2, S.singleton $ M.fromList [ (b,3), (c,4) ] ) ] )

testSubstToCondElts = TestCase $ do
  let (a,b,c,x) = (Var "a",Var "b",Var "c",Var "x")
      s = M.fromList [ (a,1), (b,2) ]
  assertBool "2" $ substToCondElts c s == Nothing
  assertBool "1" $ substToCondElts b s ==
    (Just $ M.singleton 2 $ S.singleton $ M.singleton a 1)

testVarFuncToCondElts = TestCase $ do

  let (a,b,c,x,y) = (Var "a",Var "b",Var "c",Var "x",Var "y")
      vf_a    = VarFunc a (S.empty)
      s_b1c1 = M.fromList [ (b,1), (c,1) ]
      s_b2   = M.fromList [ (b,2)        ]
      (ra :: Possible) = M.fromList [
        ( a, M.fromList [ (1, S.singleton mempty)
                        , (5, S.singleton $ M.singleton x 23) ] ) ]

  assertBool "0" $   varFuncToCondElts ra M.empty vf_a == Just ((M.!) ra a)
  assertBool "0.1" $ varFuncToCondElts ra s_b1c1  vf_a == Just ((M.!) ra a)
    -- the Subst s_b1c1 is ignored because the dets in the VarFunc are empty

  let (r :: Possible) = M.fromList
          [ ( a, M.fromList
              [ (1, S.singleton $ error "never used")
              , (2, error "doesn't matter") ] )
          , ( b, M.fromList
              [ (1, S.fromList [ M.fromList [(a, 2), (x,0)       ]
                               , M.fromList [(a, 3), (x,1)       ]
                               , M.fromList [(a, 4), (x,1)       ] ] )
              , (2, S.fromList [ M.fromList [(a,2)               ]
                               , M.fromList [(a,3) , (x,1)       ] ] ) ] )
          , ( c, M.fromList
              [ (1, S.fromList [ M.fromList [(a, 2),       (y,3) ]
                               , M.fromList [(a, 2),       (y,4) ]
                               , M.fromList [(a, 3), (x,2)       ]
                               , M.fromList [(a, 4),       (y,2) ] ] )
              , (2, error "never used, doesn't matter") ] ) ]
      aOf_b  = VarFunc a (S.fromList [b   ])
      aOf_bc = VarFunc a (S.fromList [b, c])

  assertBool "1" $ varFuncToCondElts r s_b2 aOf_b
    == Just ( M.fromList [ (2, S.singleton M.empty)
                         , (3, S.singleton $ M.singleton x 1 )
                         ] )
  assertBool "2" $ varFuncToCondElts r s_b1c1 aOf_bc
    == Just ( M.fromList [ (2, S.fromList [ M.fromList [(x,0), (y,3)]
                                          , M.fromList [(x,0), (y,4)] ] )
                         , (4, S.fromList [ M.fromList [(x,1), (y,2)] ] ) ] )


(a,b,c,x,y) = (Var "a",Var "b",Var "c",Var "x",Var "y")
vf_a    = VarFunc a (S.empty)
s_b1c1 = M.fromList [ (b,1), (c,1) ] :: Subst
s_b2   = M.fromList [ (b,2)        ] :: Subst
(ra :: Possible) = M.fromList [
  ( a, M.fromList [ (1, S.singleton mempty)
                  , (5, S.singleton $ M.singleton x 23) ] ) ]
(r :: Possible) = M.fromList
    [ ( a, M.fromList
        [ (1, S.singleton $ error "never used")
        , (2, error "doesn't matter") ] )
    , ( b, M.fromList
        [ (1, S.fromList [ M.fromList [(a, 2), (x,0)       ]
                         , M.fromList [(a, 3)              ]
                         , M.fromList [(a, 4), (x,1)       ] ] )
        , (2, S.fromList [ M.fromList [(a,2)               ]
                         , M.fromList [(a,3) , (x,1)       ] ] ) ] )
    , ( c, M.fromList
        [ (1, S.fromList [ M.fromList [(a, 2),       (y,3) ]
                         , M.fromList [(a, 2),       (y,4) ]
                         , M.fromList [(a, 3), (x,2)       ]
                         , M.fromList [(a, 4),       (y,2) ] ] )
        , (2, error "never used, doesn't matter") ] ) ]
aOf_b  = VarFunc a (S.fromList [b   ])
aOf_bc = VarFunc a (S.fromList [b, c])


testRestrictCondElts = TestCase $ do
  let (x,y,z) = (Var "x",Var "y",Var"z")
      x1    = M.fromList [ (x,1)                  ] :: Subst
      y11   = M.fromList [        (y,11)          ] :: Subst
      x1y11 = M.fromList [ (x,1), (y,11)          ] :: Subst
      xyz   = M.fromList [ (x,1), (y,11), (z,111) ] :: Subst
      x2y11 = M.fromList [ (x,2), (y,11)          ] :: Subst
      y12   = M.fromList [        (y,12)          ] :: Subst
      ces   = M.fromList [ (1, S.fromList [ x1y11
                                          , xyz ] )
                         , (2, S.singleton x2y11  ) ]
      ces'  = M.fromList [ (1, S.singleton x1)
                         , (2, S.singleton y12) ]

  assertBool "2" $ restrictCondElts (S.singleton y11) ces'
         == M.fromList [ (1, S.fromList [ x1y11 ] ) ]
  assertBool "1" $ restrictCondElts (S.singleton x1y11) ces
         == M.fromList [ (1, S.fromList [ x1y11
                                        , xyz ] ) ]
  assertBool "0" $ restrictCondElts (S.singleton y12) ces
         == M.empty

testRestrictCondElts1 = TestCase $ do
  let (x,y,z) = (Var "x",Var "y",Var"z")
      subst = M.fromList [ (x,1), (y,11) ]
      ces = M.fromList [ (1, S.fromList [ subst
                                        , M.insert z 111 subst ] )
                       , (2, S.fromList [ M.insert x 2 subst ] ) ]
  assertBool "1" $ restrictCondElts1 subst ces
         == M.fromList [ (1, S.fromList [ subst
                                        , M.insert z 111 subst ] ) ]
  -- TODO speed|memory : Notice how the z-binding is kept, whether or not
  -- it is relevant. If z is not used later, then maybe it should not be
  -- kept. This would prevent treating the Subst with the z-binding as
  -- distinct from the Subst without it, hence avoid duplicating some work.

testVarFuncSubsts = TestCase $ do
  let (a,b,c,x,y) = (Var "a",Var "b",Var "c",Var "x",Var "y")
      aOf_x  = VarFunc (a) (S.fromList [x   ])
      aOf_xy = VarFunc (a) (S.fromList [x, y])

      xCondElts = M.fromList -- x could be 1 or 2, if ...
        [ (1, S.fromList [ M.fromList [ (a, 1) ] ] )
        , (2, S.fromList [ M.fromList [ (a, 2), (b, 2) ] ] ) ]
      yCondElts = M.fromList -- y could be 3 or 4, if ...
        [ (3, S.fromList [ M.fromList [ (a, 1) ] ] )
        , (4, S.fromList [ M.fromList [         (b, 2), (c, 2) ]
                         , M.fromList [ (a, 2),         (c, 3) ]
                         , M.fromList [         (b, 3), (c, 3) ] ] ) ]
      r = M.fromList [ (x, xCondElts)
                     , (y, yCondElts) ]
      xySubst xVal yVal = M.fromList [ (x, xVal), (y, yVal) ]

  assertBool "0" $ varFuncSubsts r (xySubst 1 4) aOf_x
    == S.fromList [ M.fromList [ (a, 1) ] ]
  assertBool "1" $ varFuncSubsts r (xySubst 1 3) aOf_xy
    == S.fromList [ M.fromList [ (a, 1) ] ]
  assertBool "2" $ varFuncSubsts r (xySubst 2 3) aOf_xy
    == S.empty
  assertBool "3" $ varFuncSubsts r (xySubst 1 4) aOf_xy
    == S.fromList [ M.fromList [ (a, 1), (b, 2), (c, 2) ]
                  , M.fromList [ (a, 1), (b, 3), (c, 3) ] ]

testReconcile = TestCase $ do
  let (x,y,z) = (Var "x",Var "y",Var"z")
      x1    = S.singleton ( M.singleton x 1 )
      x1_x2 = S.fromList  [ M.singleton x 1
                          , M.singleton x 2 ]
      x1_x3 = S.fromList  [ M.singleton x 1
                          , M.singleton x 3 ]
  assertBool "1" $ reconcile (S.fromList [x1_x2, x1_x3]) == x1
  let x1y1 = S.singleton ( M.fromList [(x,1), (y,2)]        )
      y2z3 = S.singleton ( M.fromList [(x,1),        (z,3)] )
  assertBool "2" $ reconcile (S.fromList [x1, x1y1]) == x1y1
  assertBool "3" $ reconcile (S.fromList [x1, x1y1, y2z3]) ==
    S.singleton ( M.fromList [(x,1), (y,2), (z,3)] )
  assertBool "3" $ reconcile (S.fromList [x1, x1y1, y2z3, S.empty]) ==
                                                          S.empty

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

testReconcile1ToMany = TestCase $ do
  let x1 = M.singleton (Var "x") 1
      y1 = M.singleton (Var "y") 1
      y2 = M.singleton (Var "y") 2
      x1y2 = M.fromList [ ((Var "x"),1)
                        , ((Var "y"),2)]
  assertBool "1" $ reconcile1ToMany x1y2 (S.fromList [x1, y1, y2] )
                     == S.singleton x1y2
  assertBool "2" $ reconcile1ToMany x1y2 (S.singleton M.empty)
                     == S.singleton x1y2
  assertBool "3" $ reconcile1ToMany x1y2 S.empty
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

testIsSubsetOfMap = TestCase $ do
  let m  = M.fromList [ (1, S.fromList [1,2]), (2, S.fromList [2,3]) ]
      m' = M.fromList [ (1, S.fromList [1,2]), (2, S.fromList [2,4]) ]
      n  = M.fromList [ (1, S.fromList [1,2]) ]
  assertBool "1" $       isSubsetOfMap n m
  assertBool "1" $ not $ isSubsetOfMap m n
  assertBool "1" $       isSubsetOfMap n m'
  assertBool "1" $ not $ isSubsetOfMap m m'
