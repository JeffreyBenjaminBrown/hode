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
import           SeekSeq.Query
import           SeekSeq.Query.MkLeaf
import           SeekSeq.STypes
import qualified Test.Rslt.RData as D


test_module_rsltProgram = TestList [
  TestLabel "test_rslt_seekSeq" test_rslt_seekSeq
  , TestLabel "test_rslt_hash_seekSeq" test_rslt_hash_seekSeq
  ]


test_rslt_hash_seekSeq = TestCase $ do
  assertBool "<any> #like <any>" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ ExprAddr 5 ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (10, S.singleton M.empty)
                                            , (12, S.singleton M.empty) ] )

  assertBool "fish #like <any>" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ ExprAddr 5 )
             , ( RoleMember 1, HExpr $ ExprAddr 2 )
             ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (10, S.singleton M.empty) ] )

  assertBool "todo: more" False


test_rslt_seekSeq = TestCase $ do

  assertBool "1" $ runProgram D.rslt
    [ ( "a", QFind $ find
        $ \sp -> S.singleton <$> R.lookup sp (ExprAddr 0) ) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.rslt
    [ ("a", QFind $ find
        $ \sp -> S.singleton <$> refExprAt sp 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word' "") $ S.singleton M.empty )
