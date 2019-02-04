{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.THash where

import           Prelude hiding (lookup)
import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Rslt.RTypes
import Rslt.Lookup
import Rslt.Hash.HTypes
import Rslt.Hash.Hash
import Util


test_module_rslt_hash = TestList [
  TestLabel "test_pathsToIts" test_pathsToIts
  ]

test_pathsToIts = TestCase $ do
  assertBool "1" $ let
    x = M.fromList
      [ ( RoleMember 1
        , Right $ HMap $ M.fromList [ ( RoleMember 2, Left HIt ) ] )
      , ( RoleMember 3
        , Right $ HAnd $ error "irrelevant" ) ]
    in pathsToIts x == [ [ RoleMember 1, RoleMember 2 ] ]

