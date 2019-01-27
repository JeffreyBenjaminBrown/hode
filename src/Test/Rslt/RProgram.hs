module Test.Rslt.RProgram where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Program
import           Query
import           Space.Rslt
import           Space.Rslt.Index
import qualified Test.Rslt.RData as D
import           Types
import           Util


test_module_rsltProgram = TestList [
  TestLabel "test_rsltProgram" test_rsltProgram
  ]

test_rsltProgram = TestCase $ do
  assertBool "1" $ runProgram D.index
    [ ("a", QFind $ find
        $ \sp -> maybe S.empty S.singleton $ addrOf sp $ ImgOfAddr 0) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.files
    [ ("a", QFind $ find
        $ \sp -> maybe S.empty S.singleton $ M.lookup 0 sp) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word "") $ S.singleton M.empty )
