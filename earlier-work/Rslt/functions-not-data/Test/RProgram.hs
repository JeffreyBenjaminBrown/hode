{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.RProgram where

import           Data.List hiding (find)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Program
import           Query.MkLeaf
import           Space.Rslt
import           Space.Rslt.Types
import qualified Test.Rslt.RData as D
import           Types


test_module_rsltProgram = TestList [
  TestLabel "test_rsltProgram" test_rsltProgram
  ]

test_rsltProgram = TestCase $ do

  assertBool "1" $ runProgram D.rslt
    [ ( "a", QFind $ find
        $ \sp -> maybe S.empty S.singleton $ addrOf sp $ ImgOfAddr 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.rslt
    [ ("a", QFind $ find
        $ \sp -> maybe S.empty S.singleton $ exprAt sp 0) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word "") $ S.singleton M.empty )
