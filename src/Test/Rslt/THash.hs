{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.THash where

import           Prelude hiding (lookup)
import           Data.Either
import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Rslt.Edit
import Rslt.RTypes
import Rslt.Lookup
import Rslt.Hash.HTypes
import Rslt.Hash.Hash
import qualified Test.Rslt.RData as D
import Util


test_module_rslt_hash = TestList [
  TestLabel "test_pathsToIts" test_pathsToIts
  , TestLabel "test_retrieveIts" test_retrieveIts
  ]

test_retrieveIts = TestCase $ do
  let r = fromRight (error "wut") $ insertAt 7 (Rel' [5,5] 4) D.rslt
  assertBool "1" $ retrieveIts r [ [RoleMember 1, RoleMember 1] ] 7
    == Right [1]
  assertBool "2" $ retrieveIts r [ [RoleMember 2, RoleMember 2] ] 7
    == Right [2]
  assertBool "3" $ retrieveIts r [ [RoleMember 2, RoleMember 1] ] 7
    == Right [1]
  assertBool "3" $ retrieveIts r [ [RoleMember 2, RoleMember 1]
                                 , [RoleMember 1]
                                 ] 7
    == Right [1,5]
  assertBool "4" $ retrieveIts r [ [RoleMember 2, RoleMember 1]
                                 , [RoleMember 1, RoleMember 1]
                                 ] 7
    == Right [1]
  assertBool "5" $ retrieveIts r [ [RoleMember 1, RoleMember 1]
                                 , [RoleMember 1, RoleMember 2]
                                 , [RoleMember 2, RoleMember 2]
                                 ] 7
    == Right [1,2]

test_pathsToIts = TestCase $ do
  assertBool "1" $ let
    x = M.fromList
      [ ( RoleMember 1
        , Right $ HMap $ M.fromList [ ( RoleMember 2, Left HIt ) ] )
      , ( RoleMember 3
        , Right $ HAnd $ error "irrelevant" ) ]
    in pathsToIts x == [ [ RoleMember 1, RoleMember 2 ] ]

