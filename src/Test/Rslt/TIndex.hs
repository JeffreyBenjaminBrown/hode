module Test.Rslt.TIndex where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Space.Rslt
import           Space.Rslt.RTypes
import           Space.Rslt.Index
import           Space.Rslt.Index.Positions
import           Space.Rslt.Index.ImgLookup
import qualified Test.Rslt.RData as D


test_module_rslt_index = TestList [
    TestLabel "test_invertPositions" test_invertPositions
  , TestLabel "test_checkDb" test_checkDb
  ]

test_checkDb = TestCase $ do
  assertBool "1" $ M.toList (relsWithoutMatchingTplts D.badExprs D.rslt)
    == [(1001, Rel [1,2] 5), (1002, Rel [1, 2] $ -1000)]
  assertBool "2" $ M.toList (collectionsWithAbsentAddrs D.badExprs D.rslt)
    == [(1002, [-1000])]

test_invertPositions = TestCase $ do
  let ips = positionsHeldByAll [ (1,  [ (RoleMember 1, 11 )
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
