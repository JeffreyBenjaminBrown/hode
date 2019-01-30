module Test.Rslt.TIndex_and_Valid where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Data.Rslt.Lookup
import           Data.Rslt.RTypes
import           Data.Rslt.Index
import           Data.Rslt.RValid
import qualified Test.Rslt.RData as D


test_module_rslt_index_and_valid = TestList [
    TestLabel "test_invertPositions" test_invertPositions
  , TestLabel "test_checkDb" test_checkDb
  , TestLabel "test_validExpr" test_validExpr
  ]

test_validExpr = TestCase $ do
  -- TODO : test for what kind of Left, not just whether it is Left.
  -- Could do in a future-proof manner by using enum error types rather
  -- than strings, (But I checked by hand in GHCI; each `validExpr ...`
  -- expression below produces the correct kind of complaint.)
  assertBool "good Rel" $ isRight $ validExpr D.rslt (Rel [1,2] $ 4)
  assertBool "absent members" $ isLeft $ validExpr D.rslt (Rel [100,200] $ 4)
  assertBool "absent template" $ isLeft $ validExpr D.rslt (Rel [1,2] $ 44)
  assertBool "arity mismatch" $ isLeft $ validExpr D.rslt (Rel [] $ 4)
  assertBool "tplt not a tplt" $ isLeft $ validExpr D.rslt (Rel [4] $ 0)
  assertBool "word" $ isRight $ validExpr D.rslt (Word "meh")

test_checkDb = TestCase $ do
  assertBool "1" $ M.toList (relsWithoutMatchingTplts D.badExprs D.rslt)
    == [(1001, Rel [1,2] 5), (1002, Rel [1, 2] $ -1000)]
  assertBool "2" $ M.toList (collectionsWithAbsentAddrs D.badExprs D.rslt)
    == [(1002, [-1000])]

test_invertPositions = TestCase $ do
  let ips = foldl invertAndAddPositions M.empty
        [ (1,  [ (RoleMember 1, 11 )
               , (RoleMember 2, 22 ) ] )
        , (11, [ (RoleMember 1, 1  )
               , (RoleMember 2, 22 ) ] )
        , (3,  [ (RoleMember 1, 1  ) ] )
        ]
  assertBool "1" $ ips == M.fromList [(1,  S.fromList [(RoleMember 1,3  )
                                                      ,(RoleMember 1,11 )])
                                     ,(11, S.fromList [(RoleMember 1,1  )])
                                     ,(22, S.fromList [(RoleMember 2,1  )
                                                      ,(RoleMember 2,11 )])]
