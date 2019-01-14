{-# LANGUAGE ScopedTypeVariables #-}
module Test.TClassify where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Query.Classify
import Types


testModuleQueryClassify = TestList [
  TestLabel "test_quantifies" test_quantifies
  ]

test_quantifies = TestCase $ do
  let [a,b,c,x,y,z] = map Var ["a","b","c","x","y","z"]
  assertBool "1" $ quantifies (
    QOr [ ForAll x (Source x) $ QTest $ error "whatever"
        , ForAll y (Source y) $ QAnd [ ForSome z (Source z)
                                       $ QTest $ error "whatever" ] ] )
    == S.fromList [x,y,z]
