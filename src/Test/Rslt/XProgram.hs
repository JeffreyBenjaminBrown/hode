{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.XProgram where

import           Data.List hiding (find)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Program
import           Query.MkLeaf
import           Space.Xslt
import           Space.Rslt.RTypes
import qualified Test.Rslt.XData as D
import           Types


test_module_xsltProgram = TestList [
  TestLabel "test_xsltProgram" test_xsltProgram
  ]

test_xsltProgram = TestCase $ do

  assertBool "1" $ runProgram D.xslt
    [ ( "a", QFind $ find
        $ \sp -> maybe S.empty S.singleton
                 $ xImgLookup sp $ ImgOfAddr 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.xslt
    [ ("a", QFind $ find
        $ \sp -> maybe S.empty S.singleton
                 $ M.lookup 0 $ xExprs sp ) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word "") $ S.singleton M.empty )
