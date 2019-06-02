{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Qseq.TSubst where

import           Data.Either
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import qualified Test.HUnit     as T
import           Test.HUnit hiding (Test)

import Hode.Qseq.Subst
import Hode.Qseq.QTypes


vs :: String -> Var
vs = VarString

testModuleSubst :: T.Test
testModuleSubst = TestList
  [ TestLabel "testReconcile2" testReconcile2
  , TestLabel "testReconcile1ToMany" testReconcile1ToMany
  , TestLabel "testReconcile2sets" testReconcile2sets
  , TestLabel "testReconcile" testReconcile
  , TestLabel "testReconcileCondEltsAtElt" testReconcileCondEltsAtElt
  , TestLabel "testReconcileCondElts" testReconcileCondElts
  , TestLabel "testVarSubsts" testVarSubsts
  , TestLabel "testSubstToCondElts" testSubstToCondElts
  , TestLabel "testSetSubstToCondElts" testSetSubstToCondElts
  , TestLabel "test_drawVar" test_drawVar
  ]

test_drawVar :: T.Test
test_drawVar = TestCase $ do
  let p = M.fromList [ ((vs "a"), M.fromList [ (1, error "meh") ] )
                     , ((vs "b"), M.fromList [ (2, error "meh")
                                             , (3, error "meh") ] ) ]
      s = M.singleton (vs "x") 10 :: Subst Int
  assertBool "1" $ drawVar p s (vs "b") (vs "b1")
    == Right (S.fromList [ M.insert (vs "b1") 2 s
                         , M.insert (vs "b1") 3 s ] )
  assertBool "1" $ isLeft $ drawVar p s (vs "b1") (vs "b")

testSetSubstToCondElts :: T.Test
testSetSubstToCondElts = TestCase $ do
  let [a,b,c] = [(vs "a"),(vs "b"),(vs "c")]
      (s :: Subst Int) = M.fromList [ (a,1), (b,2) ]
      (t :: Subst Int) = M.fromList [ (a,1), (b,3) ]
      (u :: Subst Int) = M.fromList [ (a,1), (b,3)
                                    , (a,2), (b,3), (c,4) ]
  assertBool "1" $ setSubstToCondElts a (S.fromList [s, t,u,M.empty])
    == ( M.fromList
         [ (1, S.fromList [ M.singleton b 2
                          , M.singleton b 3 ] )
         , (2, S.singleton $ M.fromList [ (b,3), (c,4) ] ) ] )

testSubstToCondElts :: T.Test
testSubstToCondElts = TestCase $ do
  let [a,b,c] = [(vs "a"),(vs "b"),(vs "c")]
      (s :: Subst Int) = M.fromList [ (a,1), (b,2) ]
  assertBool "2" $ substToCondElts c s == Nothing
  assertBool "1" $ substToCondElts b s ==
    (Just $ M.singleton 2 $ S.singleton $ M.singleton a 1)

testVarSubsts :: T.Test
testVarSubsts = TestCase $ do
  let [a,b,c,x,y] = map vs ["a","b","c","x","y"]
      (xCondElts :: CondElts Int) = M.fromList -- x could be 1 or 2, if ...
        [ (1, S.fromList [ M.fromList [ (a, 1) ] ] )
        , (2, S.fromList [ M.fromList [ (a, 2), (b, 2) ] ] )
        , (3, S.fromList [ M.fromList [ (a, 1) ,       (c, 3) ]
                         , M.fromList [         (b, 3)        ] ] ) ]
      (yCondElts :: CondElts Int) = M.fromList -- y could be 3 or 4, if ...
        [ (3, S.fromList [ M.fromList [ (a, 1) ] ] )
        , (4, S.fromList [ M.fromList [         (b, 2), (c, 2) ]
                         , M.fromList [ (a, 2),         (c, 3) ]
                         , M.fromList [         (b, 3), (c, 3) ] ] ) ]
      r = M.fromList [ (x, xCondElts)
                     , (y, yCondElts) ]
      xySubst xVal yVal = M.fromList [ (x, xVal), (y, yVal) ]

  assertBool "0" $ reconcileDetsAcrossVars r (xySubst 1 4) (S.singleton x)
    == Right ( S.fromList [ M.fromList [ (a, 1) ] ] ) -- y irrelevant in aOf_x
  assertBool "1" $ reconcileDetsAcrossVars r (xySubst 1 3) (S.fromList [x,y])
    == Right ( S.fromList [ M.fromList [ (a, 1) ] ] )
  assertBool "2" $ reconcileDetsAcrossVars r (xySubst 2 3) (S.fromList [x,y])
    == Right ( S.empty )
  assertBool "3" $ reconcileDetsAcrossVars r (xySubst 1 4) (S.fromList [x,y])
    == Right ( S.fromList [ M.fromList [ (a, 1), (b, 2), (c, 2) ]
                          , M.fromList [ (a, 1), (b, 3), (c, 3) ] ] )
  assertBool "3" $ reconcileDetsAcrossVars r (xySubst 3 4) (S.fromList [x,y])
    == Right ( S.fromList [ M.fromList [ (a, 1), (b, 3), (c, 3) ]
  -- TODO : are the next two answers redundant? Should I keep only the 1st?
  -- If we ever need to know whether a prior value of a led to this one,
  -- that way it would be available.
                          , M.fromList [ (a, 2), (b, 3), (c, 3) ]
                          , M.fromList [         (b, 3), (c, 3) ] ] )

testReconcileCondElts :: T.Test
testReconcileCondElts = TestCase $ do
  let [a,b,c] = map vs ["a","b","c"]
      ce, cf :: CondElts Int
      ce = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 1) ]
                                       , M.fromList [ (a, 2), (b, 2) ] ] )
                      , (2, S.fromList [ M.fromList [ (a, 1), (b, 1) ] ] )
                      , (3, S.fromList [ M.empty ] ) ]
      cf = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 2) ]
                                       , M.fromList [ (a, 2), (b, 2) ] ] )
                      , (2, S.fromList [ M.fromList [ (a, 1), (c, 3) ] ] ) ]
  assertBool "1" $ reconcileCondElts (S.fromList [ce,cf])
    == M.fromList [ (1, S.singleton $ M.fromList [ (a,2), (b,2) ] )
                  , (2, S.singleton $ M.fromList [ (a,1), (b,1), (c,3) ] ) ]

testReconcileCondEltsAtElt :: T.Test
testReconcileCondEltsAtElt = TestCase $ do
  let [a,b,c] = map vs ["a","b","c"]
      ce, cf :: CondElts Int
      ce = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 1) ]
                                       , M.empty ] ) ]
      cf = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1)         ]
                                       , M.fromList [         (b, 2) ] ] ) ]

  assertBool "explanatory" $ reconcileCondEltsAtElt 1 (S.fromList [ce,cf])
    == Just ( M.singleton 1 $ S.fromList [ M.fromList [ (a,1), (b,1) ]
                                         , M.fromList [ (a,1) ]
                                         , M.fromList [ (b,2) ] ] )

  let (ce2 :: CondElts Int)
        = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 1) ]
                                      , M.fromList [ (a, 2), (b, 2) ] ] )
                     , (2, S.fromList [ M.fromList [ (a, 1), (b, 1) ] ] ) ]
      (cf2 :: CondElts Int)
        = M.fromList [ (1, S.fromList [ M.fromList [ (a, 1), (b, 2) ]
                                      , M.fromList [ (a, 2), (b, 2) ] ] )
                     , (2, S.fromList [ M.fromList [ (a, 1), (c, 3) ] ] ) ]
  assertBool "2" $ reconcileCondEltsAtElt 1 (S.fromList [ce2,cf2])
    == Just ( M.fromList
              [ (1, S.singleton $ M.fromList [ (a, 2), (b, 2) ] ) ] )
  assertBool "1" $ reconcileCondEltsAtElt 2 (S.fromList [ce2,cf2])
    == Just ( M.fromList
              [ (2, S.singleton
                  $ M.fromList [ (a,1), (b,1), (c,3) ] ) ] )

testReconcile :: T.Test
testReconcile = TestCase $ do
  let [x,y,z] = map vs ["x","y","z"]
      x1, x1_x2, x1_x3 :: Set (Subst Int)
      x1    = S.singleton ( M.singleton x 1 )
      x1_x2 = S.fromList  [ M.singleton x 1
                          , M.singleton x 2 ]
      x1_x3 = S.fromList  [ M.singleton x 1
                          , M.singleton x 3 ]
  assertBool "1" $ reconcile (S.fromList [x1_x2, x1_x3]) == x1

  let x1y2 = S.singleton ( M.fromList [(x,1), (y,2)]        )
      y2z3 = S.singleton ( M.fromList [(x,1),        (z,3)] )
  assertBool "2" $ reconcile (S.fromList [x1, x1y2]) == x1y2
  assertBool "3" $ reconcile (S.fromList [x1, x1y2, y2z3]) ==
    S.singleton ( M.fromList [(x,1), (y,2), (z,3)] )
  assertBool "3" $ reconcile (S.fromList [x1, x1y2, y2z3, S.empty]) ==
                                                          S.empty

testReconcile2sets :: T.Test
testReconcile2sets = TestCase $ do
  let x1,y1,z2,x1y2,x1z2,x1y1z2,x1y2z2 :: Subst Int
      x1 = M.singleton        (vs "x") 1
      y1 = M.singleton        (vs "y") 1
      z2 = M.singleton        (vs "z") 2
      x1y2 = M.fromList [ (   (vs "x"), 1)
                        , (   (vs "y"), 2) ]
      x1z2 = M.fromList [ (   (vs "x"), 1)
                        , (   (vs "z"), 2) ]
      x1y1z2 = M.fromList [ ( (vs "x"), 1)
                          , ( (vs "y"), 1)
                          , ( (vs "z"), 2) ]
      x1y2z2 = M.fromList [ ( (vs "x"), 1)
                          , ( (vs "y"), 2)
                          , ( (vs "z"), 2) ]
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

testReconcile1ToMany :: T.Test
testReconcile1ToMany = TestCase $ do
  let x1,y1,y2,x1y2 :: Subst Int
      x1   = M.singleton  (vs "x") 1
      y1   = M.singleton  (vs "y") 1
      y2   = M.singleton  (vs "y") 2
      x1y2 = M.fromList [ ((vs "x"), 1)
                        , ((vs "y"), 2) ]
  assertBool "1" $ reconcile1ToMany x1y2 (S.fromList [x1, y1, y2] )
                     == S.singleton x1y2
  assertBool "2" $ reconcile1ToMany x1y2 (S.singleton M.empty)
                     == S.singleton x1y2
  assertBool "3" $ reconcile1ToMany x1y2 S.empty
                     == S.empty

testReconcile2 :: T.Test
testReconcile2 = TestCase $ do
  let x1,y1,y2,x1y2 :: Subst Int
      x1 = M.singleton (vs "x") 1
      y1 = M.singleton (vs "y") 1
      y2 = M.singleton (vs "y") 2
      x1y2 = M.fromList [ (vs "x",1)
                        , (vs "y",2) ]
  assertBool "0" $ reconcile2 M.empty M.empty == Just (M.empty :: Subst Int)
  assertBool "1" $ reconcile2 M.empty x1      == Just x1
  assertBool "2" $ reconcile2 x1y2    M.empty == Just x1y2
  assertBool "3" $ reconcile2 x1      y2      == Just x1y2
  assertBool "4" $ reconcile2 y2      x1y2    == Just x1y2
  assertBool "5" $ reconcile2 y1      x1y2    == Nothing
