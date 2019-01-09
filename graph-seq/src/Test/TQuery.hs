module Test.TQuery where

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


testModuleQuery = TestList [
  TestLabel "testFindable" testFindable
  , TestLabel "testDisjointExistentials" testDisjointExistentials
  , TestLabel "testQFind" testQFind
  , TestLabel "testForSome" testForSome
  ]

testForSome = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [a,b,x,y] = map (flip Var S.empty) ["a","b","x","y"]
      aOf_b = Var "a" $ S.singleton a
      p = M.fromList
          [ ( a, M.fromList [ (1, S.singleton M.empty)
                            , (2, S.singleton M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton $ M.fromList [(a,1), (a,2)])
                            , (2, S.singleton M.empty) ] ) ]
      qc :: Var -> Query
      qc v = QFind $ findChildren $ Right v
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

testDisjointExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = Var "x" $ S.empty
      y   = Var "y" $ S.empty
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
