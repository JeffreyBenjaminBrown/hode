{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.RProgram where

import           Data.List hiding (find)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test, test)

import           Rslt.Lookup hiding (lookup)
import qualified Rslt.Lookup as R
import           Rslt.RTypes
import           Qseq.Query
import           Qseq.Query.MkLeaf
import           Qseq.QTypes
import qualified Test.Rslt.RData as D


test_module_rsltProgram = TestList [
  TestLabel "test_rslt_seekSeq" test_rslt_seekSeq
  , TestLabel "test_rslt_hash_seekSeq" test_rslt_hash_seekSeq
  ]


test_rslt_hash_seekSeq = TestCase $ do
  assertBool "<any> #like <any>" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ ExprAddr 8 ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ ( 1, S.singleton M.empty)
                                            , (10, S.singleton M.empty)
                                            , (12, S.singleton M.empty)
                                            , (19, S.singleton M.empty) ] )

  assertBool "fish #like <any>" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ ExprAddr 8 )
             , ( RoleMember 1, HExpr $ ExprAddr 2 )
             ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ ( 1, S.singleton M.empty)
                                            , (10, S.singleton M.empty) ] )

  assertBool "<any> #like (<it> #is exercise)" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ ExprAddr 8 )
             , ( RoleMember 2
               , HEval ( M.fromList
                         [ ( RoleTplt, HExpr $ ExprAddr 15)
                         , ( RoleMember 2, HExpr $ Word "exercise") ] )
                 $ [[ RoleMember 1 ]]
               ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (10, S.singleton M.empty)
                                            , (12, S.singleton M.empty) ] )

  assertBool "<it> #like (<it> #is exercise)" $ runProgram D.b2
    [ ( "a"
      , QFind $ hFind $ HEval
        ( M.fromList
          [ ( RoleTplt, HExpr $ ExprAddr 8 )
          , ( RoleMember 2
            , HEval ( M.fromList
                      [ ( RoleTplt, HExpr $ ExprAddr 15)
                      , ( RoleMember 2, HExpr $ Word "exercise") ] )
              $ [[ RoleMember 1 ]]
            ) ] )
        [[ RoleMember 1 ]] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (2, S.singleton M.empty)
                                            , (11, S.singleton M.empty) ] )

  assertBool "<it> #need <any> && <any> #need <it>" $ runProgram D.b2
    [ ( "a"
      , QFind $ hFind $ HAnd
        [ ( HEval ( M.fromList -- <it> #need <any>
                    [ ( RoleTplt, HExpr $ ExprAddr 7 ) ] )
            [[ RoleMember 1 ]] )
        , ( HEval ( M.fromList -- <any> #need <it>
                    [ ( RoleTplt, HExpr $ ExprAddr 7 ) ] )
            [[ RoleMember 2 ]] ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (2, S.singleton M.empty) ] )

  assertBool ( "another way to sayt he same thing:\n"
               ++ "a <- <it> #need <any>\n"
               ++ "b <- <any> #need <it>" )
    $ runProgram D.b2
    [ ( "a", QFind $ hFind
             ( HEval ( M.fromList -- <it> #need <any>
                       [ ( RoleTplt, HExpr $ ExprAddr 7 ) ] )
               [[ RoleMember 1 ]]  ) )

    , ( "b", ( QQuant $ ForSome "a1" "a" $ QJunct
               $ QAnd [ QFind $ hFind
                        ( HEval ( M.fromList -- <any> #need <it>
                                  [ ( RoleTplt, HExpr $ ExprAddr 7 ) ] )
                          [[ RoleMember 2 ]] )
                      , QTest $ mkTest (==) $ Right "a1" ] ) )
    ]
    == Right
    ( M.fromList
      [ ("a", M.fromList [ (2, S.singleton M.empty)
                         , (17, S.singleton M.empty) ] )
      , ("b", M.fromList [ (2, S.singleton $ M.singleton "a1" 2) ] ) ] )

  assertBool ( "nl <- everything that needs something and like something\n"
               ++ "  (i.e. <it> #like <any> && <it> #need <any>\n"
               ++ "n <- whatever any (nl) needs\n"
               ++ "l <- whatever any (nl) likes\n"
               ++ ( "res <- the subset of nl such that nothing it likes"
                    ++ " is something it needs\n" ) )
    $ runProgram D.b2
    [ ( "nl" -- "<it> #need <any> && <it> #like <any>"
      , QFind $ hFind $ HAnd
        [ ( HEval ( M.fromList -- <it> #need <any>
                    [ ( RoleTplt, HExpr $ ExprAddr 7 ) ] )
            [[ RoleMember 1 ]] )
        , ( HEval ( M.fromList -- <it> #like <any>
                    [ ( RoleTplt, HExpr $ ExprAddr 8 ) ] )
            [[ RoleMember 1 ]] ) ] )
    ]

    == Right
    ( M.fromList
      [ ("nl", M.fromList [ (2, S.singleton M.empty)
                         , (17, S.singleton M.empty) ] )
      ] )
    && False -- still to do: n, l and res
  assertBool "todo: more" False


test_rslt_seekSeq = TestCase $ do

  assertBool "1" $ runProgram D.rslt
    [ ( "a", QFind $ mkFind
        $ \sp -> S.singleton <$> R.lookup sp (ExprAddr 0) ) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.rslt
    [ ("a", QFind $ mkFind
        $ \sp -> S.singleton <$> refExprAt sp 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word' "") $ S.singleton M.empty )
