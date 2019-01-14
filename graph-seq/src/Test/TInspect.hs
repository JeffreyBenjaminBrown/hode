{-# LANGUAGE ScopedTypeVariables #-}
module Test.TInspect where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Query.Inspect
import Types


testModuleQueryClassify = TestList [
    TestLabel "test_findlike" test_findlike
  , TestLabel "test_quantifies" test_quantifies
  , TestLabel "test_disjointExistentials" test_disjointExistentials
--  , TestLabel "test_findsAndTestsOnlyQuantifiedVars" test_findsAndTestsOnlyQuantifiedVars
  ]

--test_findsAndTestsOnlyQuantifiedVars = TestCase $ do -- TODO finish
--  let [a,b,c,x,y,z] = map Var ["a","b","c","x","y","z"]
--  assertBool "1" $ usesOnlyQuantifiedVariables
--    ( ForSome v (Source v)

test_disjointExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      x   = Var "x"
      y   = Var "y"
      qx  = ForSome (Var "x") (Source x) qf
      qy  = ForSome (Var "y") (Source y) qf
      qxy = QAnd [qx,qy]
  assertBool "1" $ disjointQuantifiers (ForSome x (Source x) qx)  == False
  assertBool "2" $ disjointQuantifiers (ForSome y (Source x) qx)  == True
  assertBool "3" $ disjointQuantifiers qxy             == True
  assertBool "4" $ disjointQuantifiers (QAnd [qx,qxy]) == False

test_quantifies = TestCase $ do
  let [a,b,c,x,y,z] = map Var ["a","b","c","x","y","z"]
  assertBool "1" $ quantifies (
    QOr [ ForAll x (Source x) $ QTest $ error "whatever"
        , ForAll y (Source y) $ QAnd [ ForSome z (Source z)
                                       $ QTest $ error "whatever" ] ] )
    == S.fromList [x,y,z]

test_findlike = TestCase $ do
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
