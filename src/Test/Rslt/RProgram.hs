{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.RProgram where

import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Test.HUnit as T
import           Test.HUnit hiding (Test, test)

import           Rslt.Lookup hiding (lookup)
import qualified Rslt.Lookup as R
import           Rslt.RTypes
import           Qseq.QTypes
import           Qseq.Query
import           Qseq.MkLeaf
import qualified Test.Rslt.RData as D


test_module_rsltProgram :: T.Test
test_module_rsltProgram = TestList [
  TestLabel "test_rslt_query" test_rslt_query
  , TestLabel "test_rslt_hash_query" test_rslt_hash_query
  ]


test_rslt_hash_query :: T.Test
test_rslt_hash_query = TestCase $ do
  assertBool "<any> #like <any>" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ Addr 8 ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ ( 1, S.singleton M.empty)
                                            , (10, S.singleton M.empty)
                                            , (12, S.singleton M.empty)
                                            , (19, S.singleton M.empty) ] )

  assertBool "fish #like <any>" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ Addr 8 )
             , ( RoleMember 1, HExpr $ Addr 2 )
             ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ ( 1, S.singleton M.empty)
                                            , (10, S.singleton M.empty) ] )

  assertBool "<any> #like (<it> #is exercise)" $ runProgram D.b2
    [ ( "a", QFind $ hFind $ HMap $ M.fromList
             [ ( RoleTplt, HExpr $ Addr 8 )
             , ( RoleMember 2
               , HEval ( HMap $ M.fromList
                         [ ( RoleTplt, HExpr $ Addr 15)
                         , ( RoleMember 2, HExpr $ Word "exercise") ] )
                 $ [[ RoleMember 1 ]]
               ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (10, S.singleton M.empty)
                                            , (12, S.singleton M.empty) ] )

  assertBool "<it> #like (<it> #is exercise)" $ runProgram D.b2
    [ ( "a"
      , QFind $ hFind $ HEval
        ( HMap $ M.fromList
          [ ( RoleTplt, HExpr $ Addr 8 )
          , ( RoleMember 2
            , HEval
              ( HMap $ M.fromList
                [ ( RoleTplt, HExpr $ Addr 15)
                , ( RoleMember 2, HExpr $ Word "exercise") ] )
              $ [[ RoleMember 1 ]]
            ) ] )
        [[ RoleMember 1 ]] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (2, S.singleton M.empty)
                                            , (11, S.singleton M.empty) ] )

  assertBool "<it> #need <any> && <any> #need <it>" $ runProgram D.b2
    [ ( "a"
      , QFind $ hFind $ HAnd
        [ ( HEval ( HMap $ M.fromList -- <it> #need <any>
                    [ ( RoleTplt, HExpr $ Addr 7 ) ] )
            [[ RoleMember 1 ]] )
        , ( HEval ( HMap $ M.fromList -- <any> #need <it>
                    [ ( RoleTplt, HExpr $ Addr 7 ) ] )
            [[ RoleMember 2 ]] ) ] ) ]
    == Right ( M.singleton "a" $ M.fromList [ (2, S.singleton M.empty) ] )

  assertBool ( "another way to sayt he same thing:\n"
               ++ "a <- <it> #need <any>\n"
               ++ "b <- <any> #need <it>" )
    $ runProgram D.b2
    [ ( "a", QFind $ hFind
             ( HEval ( HMap $ M.fromList -- <it> #need <any>
                       [ ( RoleTplt, HExpr $ Addr 7 ) ] )
               [[ RoleMember 1 ]]  ) )

    , ( "b", ( QQuant $ ForSome "a1" "a" $ QJunct
               $ QAnd [ QFind $ hFind
                        ( HEval
                          ( HMap $ M.fromList -- <any> #need <it>
                            [ ( RoleTplt, HExpr $ Addr 7 ) ] )
                          [[ RoleMember 2 ]] )
                      , QTest $ mkTest (==) $ Right "a1" ] ) )
    ]
    == Right
    ( M.fromList
      [ ("a", M.fromList [ (2, S.singleton M.empty)
                         , (17, S.singleton M.empty) ] )
      , ("b", M.fromList [ (2, S.singleton $ M.singleton "a1" 2) ] ) ] )

  assertBool ( "nl <- everything that needs something and likes something\n"
               ++ "  (i.e. <it> #like <any> && <it> #need <any>\n"
               ++ "n <- whatever any (nl) needs\n"
               ++ "l <- whatever any (nl) likes\n"
               ++ ( "res <- the subset of nl such that nothing it likes"
                    ++ " is something it needs\n" ) )
    $ runProgram D.b2
      [ ( "nl" -- "<it> #need <any> && <it> #like <any>"
          , QFind $ hFind $ HAnd
            [ ( HEval
                ( HMap $ M.singleton RoleTplt $ HExpr $ Addr 7 )
                [[ RoleMember 1 ]] )
            , ( HEval
                ( HMap $ M.singleton RoleTplt $ HExpr $ Addr 8 )
                [[ RoleMember 1 ]] ) ] )

        , ( "n" -- for all nl1 in nl, "x #need <it>"
          , QQuant $ ForSome "nl1" "nl" $ QFind $ hFind $ HEval
            ( HMap $ M.fromList [ ( RoleTplt, HExpr $ Addr 7 )
                                , ( RoleMember 1, HVar "nl1" ) ] )
            [[ RoleMember 2 ]] )

        , ( "l" -- for all nl1 in nl, "x #like <it>"
          , QQuant $ ForSome "nl1" "nl" $ QFind $ hFind $ HEval
            ( HMap $ M.fromList [ ( RoleTplt, HExpr $ Addr 8 )
                                , ( RoleMember 1, HVar "nl1" ) ] )
            [[ RoleMember 2 ]] )

        , ( "res" -- for all nl1 in nl, no n(nl1) is equal to any l(nl1)
          -- If I uncomment the QQuants and the mkVTestCompare, it finds things.
          , QQuant $ ForSome "nl2" "nl"
            $ QJunct $ QAnd
            [ QFind $ hFind $ HVar "nl2"
            , QQuant $ ForAll "n1" "n"
              ( QVTest $ mkVTestIO' ("nl2","nl1") ("n1","n") )
              $ QQuant $ ForAll "l1" "l"
              ( QVTest $ mkVTestIO' ("nl2","nl1") ("l1","l") )
              $ QVTest $ mkVTestCompare (/=) (Right "n1") (Right "l1")
            ] )
        ]

    == Right
    ( M.fromList
      [ ("nl", M.fromList [ (2, S.singleton M.empty) -- fish
                         , (17, S.singleton M.empty) ] ) -- dolphins
      , ("n", M.fromList
          [ ( 3, S.singleton $ M.singleton "nl1" 2) -- water (fish need it)
          , ( 2 -- fish (dolphins need it)
            , S.singleton $ M.singleton "nl1" 17) ] )
      , ("l", M.fromList
          [ ( 3, S.fromList
                 [ M.singleton "nl1" 2 -- water (fish like it)
                 , M.singleton "nl1" 17 ] ) -- water (dolphins like it)
          , ( 6, S.singleton $ M.singleton "nl1" 2) -- jumping (fish like it)
          ] )
      , ("res", M.singleton 17 $ S.singleton $ M.singleton "nl2" 17 )
      -- dolphins are the only thing that likes something, needs something,
      -- and likes nothing that it needs
      ] )

test_rslt_query :: T.Test
test_rslt_query = TestCase $ do

  assertBool "1" $ runProgram D.rslt
    [ ( "a", QFind $ mkFind
        $ \sp -> S.singleton <$> R.lookup sp (Addr 0) ) ]
    == Right ( M.singleton "a"
               $ M.singleton 0 $ S.singleton M.empty )

  assertBool "2" $ runProgram D.rslt
    [ ("a", QFind $ mkFind
        $ \sp -> S.singleton <$> refExprAt sp 0 ) ]
    == Right ( M.singleton "a"
               $ M.singleton (Word' "") $ S.singleton M.empty )
