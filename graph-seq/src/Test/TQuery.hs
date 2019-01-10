{-# LANGUAGE ScopedTypeVariables #-}
module Test.TQuery where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import Test.HUnit

import Graph
import Query
import Query.Classify
import Types


testModuleQuery = TestList [
    TestLabel "testFindable" testFindable
  , TestLabel "testOkExistentials" testOkExistentials
  , TestLabel "testQFind" testQFind
  , TestLabel "testForSome" testForSome
  , TestLabel "testForAll" testForAll
  , TestLabel "testQAnd" testQAnd
  ]

testQAnd = TestCase $ do
  let g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      [a,b,c,x,y] = map (flip Var S.empty) ["a","b","c","x","y"]
  assertBool "todo" False

testForAll = TestCase $ do
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
  assertBool "4" $ runQuery g p (ForAll aOf_c $ qc aOf_c) (M.singleton c 1)
    == M.fromList [ (12, S.singleton $ M.singleton c 1)
                  , (22, S.singleton $ M.singleton c 1) ]
  assertBool "3" $ runQuery g p (ForAll b $ qc b) (M.singleton x 1)
    == M.fromList [ (12, S.singleton M.empty) ]
  assertBool "3" $ runQuery g p (ForAll b $ qc b) (M.singleton x 1)
    == M.fromList [ (12, S.singleton M.empty) ]
  assertBool "2" $ runQuery g p (ForAll b $ qc b) M.empty
    == M.fromList [ (12, S.singleton M.empty) ]
  assertBool "1" $ runQuery g p (ForAll a $ qc a) M.empty
    == M.fromList [ (12, S.singleton M.empty) ]

testForSome = TestCase $ do
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
  assertBool "3" $ (runQuery g p (ForSome aOf_b $ qc aOf_b) $ M.singleton b 1)
    == M.fromList [ (11, S.singleton $ M.fromList [ (b, 1), (aOf_b, 1) ] )
                  , (21, S.singleton $ M.fromList [ (b, 1), (aOf_b, 1) ] ) ]
  assertBool "2" $ (runQuery g p (ForSome aOf_b $ qc aOf_b) $ M.singleton b 2)
    == M.empty
  assertBool "1" $ runQuery g p (ForSome a $ qc a) M.empty
    == M.fromList [ (11, S.singleton $ M.singleton a 1)
                  , (21, S.singleton $ M.singleton a 1)
                  , (12, S.singleton $ M.singleton a 2)
                  , (22, S.singleton $ M.singleton a 2) ]

testQFind = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [x,y] = map (flip Var S.empty) ["x","y"]
      f1 = QFind $ findChildren $ Left 1
      fy = QFind $ findChildren $ Right y
      s = M.fromList [(x,1), (y,2)] :: Subst
  assertBool "1" $ runQuery g M.empty f1 M.empty
    == M.fromList [ (11, S.singleton M.empty)
                  , (21, S.singleton M.empty) ]
  assertBool "2" $ runQuery g M.empty fy s
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
