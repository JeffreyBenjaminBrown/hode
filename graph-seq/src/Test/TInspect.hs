{-# LANGUAGE ScopedTypeVariables #-}
module Test.TInspect where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Graph
import Query.Inspect
import Types


testModuleQueryClassify = TestList [
    TestLabel "test_findlike" test_findlike
  , TestLabel "test_quantifies" test_quantifies
  , TestLabel "test_disjointExistentials" test_disjointExistentials
  , TestLabel "test_internalAndExternalVars" test_internalAndExternalVars
  -- these seem too easy to bother testing:
    -- validQuery
    -- feasible'Junctions
    -- findsAndTestsOnlyQuantifiedVars
  ]

test_internalAndExternalVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      -- These queries are named for their internal and external variables.
      c_ade = ForSome c (Source' d $ S.singleton a)
        $ QFind $ findParents $ Right e
      c_ag = ForSome c (Source' a $ S.singleton g)
        $ QFind $ findParents $ Right c
  assertBool "5" $ internalAndExternalVars (QOr [ c_ade, c_ag ])
    == (S.singleton c, S.fromList [a,d,e,g] )
  assertBool "4" $ internalAndExternalVars c_ade
    == (S.singleton c, S.fromList [a,d,e])
  assertBool "3" $ internalAndExternalVars c_ag
    == (S.singleton c, S.fromList [a,g])
  assertBool "2" $ internalAndExternalVars (QFind $ findParents $ Right c )
    == (S.empty, S.singleton c)
  assertBool "1" $ internalAndExternalVars
    ( ForAll a (Source b) $ QOr [ c_ade, c_ag ] )
    == ( S.fromList [a,c ], S.fromList [b,d,e,g ] )

test_disjointExistentials = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      [x,x1,x2,y,y1,y2] = ["x","x1","x2","y","y1","y2"]
      qx  = ForSome  "x"  (Source x) qf
      qy  = ForSome  "y"  (Source y) qf
      qx1  = ForSome "x1" (Source x) qf
      qy1  = ForSome "y1" (Source y) qf
      qxy = QOr [qx1,qy1]
  assertBool "-1" $ disjointQuantifiers (ForSome x (Source y) qx)  == False
  assertBool "0" $ disjointQuantifiers (ForSome x (Source x) qy)   == False
  assertBool "1" $ disjointQuantifiers (ForSome x2 (Source y) qx1) == True
  assertBool "2" $ disjointQuantifiers (ForSome y (Source x) qx1)  == True
  assertBool "3" $ disjointQuantifiers qx                          == False
  assertBool "3.5" $ disjointQuantifiers qx1                       == True
  assertBool "3.7" $ disjointQuantifiers qxy                       == True
  assertBool "4" $ disjointQuantifiers (QAnd [qx1,qxy])            == False

test_quantifies = TestCase $ do
  let [a,b,c,x,y,z] = ["a","b","c","x","y","z"]
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
