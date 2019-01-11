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
    TestLabel "testFindable" testFindable
  , TestLabel "testOkExistentials" testOkExistentials
  , TestLabel "test_runFindable_Find" test_runFindable_Find
  , TestLabel "testRunTest" testRunTest
  , TestLabel "test_runFindable_ForSome" test_runFindable_ForSome
  , TestLabel "test_runFindable_ForAll" test_runFindable_ForAll
  , TestLabel "testRunTestable" testRunTestable
  , TestLabel "testRunQAnd" testRunQAnd
  , TestLabel "test_runFindable_mixed" test_runFindable_mixed
  ]

test_runFindable_mixed = TestCase $ do
  let [a,b,c,x,y] = map (flip Var S.empty) ["a","b","c","x","y"]
      aOf_b = Var "a" $ S.singleton $ Var "b" S.empty

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
      q_And_Quant = QAnd [ ForAll aOf_b $ isnt aOf_b
                         , ForSome aOf_b $ fc aOf_b ]
      q_ForAll_And = ForAll aOf_b $ QAnd [ fc0, isnt a ]

  assertBool "2" $ runFindable d p q_And_Quant s
    == M.singleton 4 (S.fromList [ M.singleton aOf_b 2
                                 , M.singleton b 23 ] )
  assertBool "1" $ runFindable d p q_ForAll_And s
    == M.singleton 1 (S.singleton $ M.singleton a 2)

testRunQAnd = TestCase $ do
  let [a,b,c,x,y] = map (flip Var S.empty) ["a","b","c","x","y"]
      (a2 :: Subst) = M.singleton a 2
      nota = QTest $ isNot $ Right a
      d = graph [ (0, [1,2    ] )
                , (3, [  2,3,4] ) ]
      (p :: Possible) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (3, S.singleton M.empty) ]
      fc0 = QFind $ findChildren $ Left 0
  assertBool "1" $ runQAnd d p [fc0, nota] a2
    == M.singleton 1 (S.singleton $ M.singleton a 2)

testRunTestable = TestCase $ do
  let [a,b,c,x,y] = map (flip Var S.empty) ["a","b","c","x","y"]
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
  assertBool "5" $ runTestable d p (ForAll a ( QTest nota )) M.empty ce
       == M.singleton 3 (S.singleton M.empty)
  assertBool "4" $ let qfa = ForSome a ( QAnd [QTest not3, QTest nota] )
                       ce23 = M.restrictKeys ce $ S.singleton 2
    in runTestable d p qfa M.empty ce23
       == M.singleton 2 (S.singleton $ M.singleton a 1)
  assertBool "3" $ runTestable d M.empty (QOr  [QTest not3,QTest nota]) a2 ce
    == M.fromList [ (1, S.fromList [ M.empty, M.singleton a 2 ] )
                  , (2, S.fromList [ M.empty                  ] )
                  , (3, S.fromList [          M.singleton a 2 ] ) ]
  assertBool "2" $ runTestable d M.empty (QAnd [QTest not3,QTest nota]) a2 ce
    == M.fromList ( map (, S.singleton $ M.singleton a 2) [1  ] )
  assertBool "1" $ runTestable d M.empty (QTest nota)                   a2 ce
    == M.fromList ( map (, S.singleton $ M.singleton a 2) [1,3] )

test_runFindable_ForAll = TestCase $ do
  let g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      [a,b,c,x,y] = map (flip Var S.empty) ["a","b","c","x","y"]
      aOf_c = a {varDets = S.singleton c}
      (p :: Possible) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton $ M.singleton x 1) ] )
          , ( c, M.fromList [ (1, S.singleton $ M.singleton a 2) ] ) ]
      qc :: Var -> Query
      qc v = QFind $ findChildren $ Right v
  assertBool "4" $ runFindable g p (ForAll aOf_c $ qc aOf_c) (M.singleton c 1)
    == M.fromList [ (12, S.singleton $ M.singleton c 1)
                  , (22, S.singleton $ M.singleton c 1) ]
  assertBool "3" $ runFindable g p (ForAll b $ qc b) (M.singleton x 1)
    == M.fromList [ (12, S.singleton M.empty) ]
  assertBool "3" $ runFindable g p (ForAll b $ qc b) (M.singleton x 1)
    == M.fromList [ (12, S.singleton M.empty) ]
  assertBool "2" $ runFindable g p (ForAll b $ qc b) M.empty
    == M.fromList [ (12, S.singleton M.empty) ]
  assertBool "1" $ runFindable g p (ForAll a $ qc a) M.empty
    == M.fromList [ (12, S.singleton M.empty) ]

test_runFindable_ForSome = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [a,b,x,y] = map (flip Var S.empty) ["a","b","x","y"]
      aOf_b = Var "a" $ S.singleton b
      p = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton $ M.singleton a 1)
                            , (2, S.singleton   M.empty) ] ) ] :: Possible
      qc :: Var -> Query
      qc v = QFind $ findChildren $ Right v
  assertBool "3" $ (runFindable g p (ForSome aOf_b $ qc aOf_b) $ M.singleton b 1)
    == M.fromList [ (11, S.singleton $ M.fromList [ (b, 1), (aOf_b, 1) ] )
                  , (21, S.singleton $ M.fromList [ (b, 1), (aOf_b, 1) ] ) ]
  assertBool "2" $ (runFindable g p (ForSome aOf_b $ qc aOf_b) $ M.singleton b 2)
    == M.empty
  assertBool "1" $ runFindable g p (ForSome a $ qc a) M.empty
    == M.fromList [ (11, S.singleton $ M.singleton a 1)
                  , (21, S.singleton $ M.singleton a 1)
                  , (12, S.singleton $ M.singleton a 2)
                  , (22, S.singleton $ M.singleton a 2) ]

testRunTest = TestCase $ do
  let [a,b,c,x,y] = map (flip Var S.empty) ["a","b","c","x","y"]
      g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      (a2 :: Subst) = M.singleton a 2
      (ce :: CondElts) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                    , (2, S.empty) ]
  assertBool "1" $ runTest g a2 (isNot $ Left 1) ce
    == M.singleton 2 (S.singleton M.empty)
  assertBool "2" $ runTest g a2 (isNot $ Right a) ce
    == M.singleton 1 ( S.singleton $ M.fromList [(a,2)] )

test_runFindable_Find = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [x,y] = map (flip Var S.empty) ["x","y"]
      f1 = QFind $ findChildren $ Left 1
      fy = QFind $ findChildren $ Right y
      s = M.fromList [(x,1), (y,2)] :: Subst
  assertBool "1" $ runFindable g M.empty f1 M.empty
    == M.fromList [ (11, S.singleton M.empty)
                  , (21, S.singleton M.empty) ]
  assertBool "2" $ runFindable g M.empty fy s
    == M.fromList [ (12, S.singleton $ M.singleton y 2)
                  , (22, S.singleton $ M.singleton y 2) ]

testOkExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = Var "x" $ S.empty
      y   = Var "y" $ S.empty
      qx  = ForSome x qf
      qy  = ForSome y qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ okExistentials (ForSome x qx)  == False
  assertBool "2" $ okExistentials (ForSome y qx)  == True
  assertBool "3" $ okExistentials qxy             == True
  assertBool "4" $ okExistentials (QAnd [qx,qxy]) == False

testFindable = TestCase $ do
  let qf = QFind $ Find (\_ _    -> S.empty) S.empty
      qc = QTest $ Test (\_ _  _ -> False  ) S.empty
  assertBool "1" $ findable (QAnd [qf, qc]) == True
  assertBool "2" $ findable (QAnd [qf]    ) == True
  assertBool "3" $ findable (QAnd [qc]    ) == False
  assertBool "4" $ findable (QAnd []      ) == False
  assertBool "5" $ findable (QOr  [qf, qc]) == False
  assertBool "6" $ findable (QOr  [qf]    ) == True
  assertBool "7" $ findable (QOr  [qc]    ) == False
  assertBool "8" $ findable (QOr  []      ) == False
