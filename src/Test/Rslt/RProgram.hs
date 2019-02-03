{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.RProgram where

import           Data.List hiding (find)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Rslt.Lookup hiding (lookup)
import qualified Rslt.Lookup as R
import           Rslt.RTypes
import           Search.Program
import           Search.Query.MkLeaf
import           Search.Types
import qualified Test.Rslt.RData as D


test_module_rsltProgram = TestList [
  TestLabel "test_rsltProgram" test_rsltProgram
  ]

test_rsltProgram = TestCase $ do

  assertBool "1" $ runProgram D.rslt
    [ ( "a", QFind $ find
        $ \sp -> either (const S.empty) S.singleton
                 $ R.lookup sp $ ExprAddr 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.rslt
    [ ("a", QFind $ find
        $ \sp -> either (const S.empty) S.singleton
                 $ refExprAt sp 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word' "") $ S.singleton M.empty )
