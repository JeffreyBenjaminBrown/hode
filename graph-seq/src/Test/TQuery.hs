{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Test.TQuery where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit hiding (Test)

import Graph
import Query
import Query.Classify
import Types


testModuleQuery = TestList [
    TestLabel "testFindlike" testFindlike
  , TestLabel "testOkExistentials" testOkExistentials
  , TestLabel "test_runFindlike_Find" test_runFindlike_Find
  , TestLabel "testRunTest" testRunTest
  , TestLabel "test_runFindlike_ForSome" test_runFindlike_ForSome
  , TestLabel "test_runFindlike_ForAll" test_runFindlike_ForAll
  , TestLabel "testRunTestlike" testRunTestlike
  , TestLabel "testRunQAnd" testRunQAnd
  , TestLabel "test_runFindlike_mixed" test_runFindlike_mixed
  ]

test_runFindlike_mixed = TestCase $ do
  let [a,b,c,x,y] = map Var ["a","b","c","x","y"]
      aOf_b = Var "a"
      src_aOf_b = Source' a $ S.singleton b

      isnt v = QTest $ isNot $ Right v
      d = graph [ (0, [1,2        ] )
                , (3, [  2,3,4    ] )
                , (10,[11, 23     ] ) ]
      (p :: Possible) = M.fromList
        [ (a, M.fromList $ map (, S.singleton M.empty) [1,2,3])
        , (b, M.fromList [ (11, S.singleton $ M.singleton a 1)
                         , (23, S.fromList [ M.singleton a 2
                                           , M.singleton a 3 ] ) ] ) ]
      fc0 = QFind $ findChildren $ Left 0
      fc v = QFind $ findChildren $ Right v
      s = M.fromList [(a,2), (b,23)] :: Subst
      q_And_Quant = let aOf_b' = aOf_b {varName = "a1"}
        in QAnd [ ForAll aOf_b' src_aOf_b $ isnt aOf_b'
                , ForSome aOf_b src_aOf_b $ fc aOf_b ]
      q_ForAll_And = ForAll aOf_b src_aOf_b $ QAnd [ fc0, isnt a ]

  assertBool "2" $ runFindlike d p q_And_Quant s
    == Right ( M.singleton 4 (S.fromList [ M.singleton aOf_b 3 ] ) )
  assertBool "1" $ runFindlike d p q_ForAll_And s
    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )

testRunQAnd = TestCase $ do
  let [a,b,c,x,y] = map Var ["a","b","c","x","y"]
      (a2 :: Subst) = M.singleton a 2
      nota = QTest $ isNot $ Right a
      d = graph [ (0, [1,2    ] )
                , (3, [  2,3,4] ) ]
      (p :: Possible) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (3, S.singleton M.empty) ]
      fc0 = QFind $ findChildren $ Left 0
  assertBool "1" $ runQAnd d p [fc0, nota] a2
    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )

testRunTestlike = TestCase $ do
  let [a,b,c,x,y] = map Var ["a","b","c","x","y"]
      (a2 :: Subst) = M.singleton a 2
      (not3 :: Test) = isNot $ Left 3
      (nota :: Test) = isNot $ Right a
      d = graph $ map (,[]) [1..3]
      (p :: Possible) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (2, S.singleton M.empty) ]
      (ce :: CondElts) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                    , (2, S.singleton M.empty)
                                    , (3, S.singleton M.empty) ]
  assertBool "5" $
    runTestlike d p (ForAll a (Source a) $ QTest nota) M.empty ce
       == Right ( M.singleton 3 (S.singleton M.empty) )
  assertBool "4" $ let
    qfa = ForSome a (Source a) $ QAnd [QTest not3, QTest nota]
    ce23 = M.restrictKeys ce $ S.singleton 2
    in runTestlike d p qfa M.empty ce23
       == Right ( M.singleton 2 (S.singleton $ M.singleton a 1) )
  assertBool "3" $ runTestlike d M.empty (QOr  [QTest not3,QTest nota]) a2 ce
    == Right ( M.fromList
               [ (1, S.fromList [ M.fromList [(a,2), (x,0)]
                                , M.singleton x 0           ] )
               , (2, S.fromList [ M.empty                   ] )
               , (3, S.fromList [          M.singleton a 2  ] ) ] )
  assertBool "2" $ runTestlike d M.empty (QAnd [QTest not3,QTest nota]) a2 ce
    == Right ( M.singleton 1 $ S.singleton $ M.fromList [(a,2), (x,0)] )
  assertBool "1" $ runTestlike d M.empty (QTest nota)                   a2 ce
    == Right ( M.fromList [ (1, S.singleton $ M.fromList [(a,2), (x,0)] )
                          , (3, S.singleton $ M.singleton a 2) ] )

test_runFindlike_ForAll = TestCase $ do
  let g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      [a,b,c,x,y] = map Var ["a","b","c","x","y"]
      aOf_c = Var "aOf_c"
      src_aOf_c = Source' a $ S.singleton c
      (p :: Possible) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton $ M.singleton x 1) ] )
          , ( c, M.fromList [ (1, S.singleton $ M.singleton a 2) ] ) ]
      qc :: Var -> Query
      qc v = QFind $ findChildren $ Right v

  assertBool "4" $
    runFindlike g p (ForAll aOf_c src_aOf_c $ qc aOf_c) (M.singleton c 1)
    == Right ( M.fromList [ (12, S.singleton $ M.empty)
                          , (22, S.singleton $ M.empty) ] )
  assertBool "3" $
    runFindlike g p (ForAll b (Source b) $ qc b) (M.singleton x 1)
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
  assertBool "3" $
    runFindlike g p (ForAll b (Source b) $ qc b) (M.singleton x 1)
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
  assertBool "2" $ runFindlike g p (ForAll b (Source b) $ qc b) M.empty
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
  assertBool "1" $ runFindlike g p (ForAll a (Source a) $ qc a) M.empty
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

test_runFindlike_ForSome = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [a,b,x,y] = map Var ["a","b","x","y"]
      aOf_b = Var "aOf_b"
      src_aOf_b = Source' a $ S.singleton b
      p = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton $ M.singleton a 1)
                            , (2, S.singleton   M.empty) ] ) ] :: Possible
      qc :: Var -> Query
      qc v = QFind $ findChildren $ Right v

  assertBool "3" $
    (runFindlike g p (ForSome aOf_b src_aOf_b $ qc aOf_b) $ M.singleton b 1)
    == Right (M.fromList [ (11, S.singleton $ M.fromList [ (aOf_b, 1) ] )
                         , (21, S.singleton $ M.fromList [ (aOf_b, 1) ] ) ] )
  assertBool "2" $
    (runFindlike g p (ForSome aOf_b src_aOf_b $ qc aOf_b) $ M.singleton b 2)
    == Right M.empty
  assertBool "1" $ runFindlike g p (ForSome a (Source a) $ qc a) M.empty
    == Right ( M.fromList [ (11, S.singleton $ M.singleton a 1)
                          , (21, S.singleton $ M.singleton a 1)
                          , (12, S.singleton $ M.singleton a 2)
                          , (22, S.singleton $ M.singleton a 2) ] )

testRunTest = TestCase $ do
  let [a,b,c,x,y] = map Var ["a","b","c","x","y"]
      g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      (a2 :: Subst) = M.singleton a 2
      (ce :: CondElts) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                    , (2, S.singleton $ M.empty) ]
  assertBool "1" $ runTest g a2 (isNot $ Left 1) ce
    == M.singleton 2 (S.singleton M.empty)
  assertBool "2" $ runTest g a2 (isNot $ Right a) ce
    == M.singleton 1 ( S.singleton $ M.fromList [(a,2), (x,0)] )

test_runFindlike_Find = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [x,y] = map Var ["x","y"]
      f1 = QFind $ findChildren $ Left 1
      fy = QFind $ findChildren $ Right y
      s = M.fromList [(x,1), (y,2)] :: Subst
  assertBool "1" $ runFindlike g M.empty f1 M.empty
    == Right ( M.fromList [ (11, S.singleton M.empty)
                  , (21, S.singleton M.empty) ] )
  assertBool "2" $ runFindlike g M.empty fy s
    == Right ( M.fromList [ (12, S.singleton $ M.singleton y 2)
                          , (22, S.singleton $ M.singleton y 2) ] )

testOkExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = Var "x"
      y   = Var "y"
      qx  = ForSome (Var "x") (Source x) qf
      qy  = ForSome (Var "y") (Source y) qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ disjointExistentials (ForSome x (Source x) qx)  == False
  assertBool "2" $ disjointExistentials (ForSome y (Source x) qx)  == True
  assertBool "3" $ disjointExistentials qxy             == True
  assertBool "4" $ disjointExistentials (QAnd [qx,qxy]) == False

testFindlike = TestCase $ do
  let qf = QFind $ Find (\_ _    -> S.empty) S.empty
      qc = QTest $ Test (\_ _  _ -> False  ) S.empty
  assertBool "1" $ findlike (QAnd [qf, qc]) == True
  assertBool "2" $ findlike (QAnd [qf]    ) == True
  assertBool "3" $ findlike (QAnd [qc]    ) == False
  assertBool "4" $ findlike (QAnd []      ) == False
  assertBool "5" $ findlike (QOr  [qf, qc]) == False
  assertBool "6" $ findlike (QOr  [qf]    ) == True
  assertBool "7" $ findlike (QOr  [qc]    ) == False
  assertBool "8" $ findlike (QOr  []      ) == False
