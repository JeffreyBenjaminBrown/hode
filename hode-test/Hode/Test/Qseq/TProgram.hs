{-# LANGUAGE ScopedTypeVariables #-}
module Hode.Test.Qseq.TProgram where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Test.HUnit     as T
import           Test.HUnit hiding (Test, test)

import Hode.Qseq.Query
import Hode.Qseq.MkLeaf
import Hode.Data.Graph
import Hode.Qseq.Types


vs :: String -> Var
vs = VarString

test_module_Program :: T.Test
test_module_Program = TestList [
  TestLabel "test_runProgram" test_runProgram
  , TestLabel "test_runNestedQuants" test_runNestedQuants
  ]

test_runNestedQuants :: T.Test
test_runNestedQuants = TestCase $ do
  let [a0,a1] = map vs ["a0","a1"]

  assertBool ( "every c for which all of c's children "
               ++ "which are also 3's children are < 10" ) $
    let d = mkGraph [ (1, [  4,40     ] )
                    , (2, [  2,20     ] )
                    , (3, [  2,3,30   ] ) ] :: Graph Int
        res = runProgram d
                [ ( (vs "all"), QFind $ mkFindReturn' $ graphNodes d )
                , ( (vs "children"), QQuant $ ForSome a0 (vs "all")
                                     $ QFind $ findChildren $ Right a0 )
                , ( (vs "children of 3"), QFind $ findChildren $ Left 3)
                , ( lastKey
                  , QQuant $ ForSome a1 (vs "all")
                    $ QJunct $ QAnd
                    [ QFind $ mkFindReturn $ Right a1
                    , QVTest $ mkVTestCompare (<) (Right a1) $ Left 10
                    , ( -- this query is varTestlike
                        QQuant $ ForAll (vs "c of a1") (vs "children")
                        ( -- restrict to children of a1
                          QVTest $ mkVTestIO' (a1,a0) $
                          (vs "c of a1", vs "children") )
                        $ QQuant $ ForAll (vs "c of 3") (vs "children of 3")
                        ( QJunct $ QAnd [] )
                        $ QVTest ( mkVTestCompare (/=)
                                   (Right $ vs "c of a1")
                                   (Right $ vs "c of 3") ) )
                    ] ) ]
        lastKey = (vs "under 10 and its children don't overlap those of 3")
    in M.lookup lastKey (fromRight (error "donkeys") res)
       == Just ( M.fromList [ (1, S.singleton $ M.singleton a1 1)
                            , (4, S.singleton $ M.singleton a1 4) ] )

test_runProgram :: T.Test
test_runProgram = TestCase $ do
  let [a,b] = map vs ["a","b"]
      [a2]  = map vs ["a2"]
      d = mkGraph [ (0, [1,2        ] )
                  , (3, [  2,3,4    ] )
                  , (10,[11, 23     ] ) ]

  assertBool "1" $ runProgram d [ (a, QFind $ findParents $ Left 2) ]
    == Right ( M.singleton a ( M.fromList [ (0, S.singleton M.empty)
                                          , (3, S.singleton M.empty) ] )
             :: Possible Int )

  assertBool "2" $ runProgram d
    [ ( b, ( QJunct $ QAnd [ QFind $ findChildren $ Left 3
                             , QTest $ mkTest (/=) $ Left 2 ] ) ) ]
    == Right (M.singleton b ( M.fromList [ (3, S.singleton M.empty)
                                           , (4, S.singleton M.empty) ] ) )

  assertBool "3" $ runProgram d
    [ ( a, QFind $ findParents $ Left 2)
    , ( b, ( QQuant $ ForSome a a $
               QJunct $ QAnd
               [ QFind $ findChildren $ Right a
               , QTest $ mkTest (/=) $ Right a
               , QTest $ mkTest (/=) $ Left 2 ] ) ) ]
    == Right ( M.fromList
               [ ( a, M.fromList [ (0, S.singleton M.empty)
                                 , (3, S.singleton M.empty) ] )
               , ( b, M.fromList [ (1, S.singleton $ M.singleton a 0)
                                 , (4, S.singleton $ M.singleton a 3)
                                 ] ) ] )

  let d2 = mkGraph [ (0, [1,2,3] )
                  , (1, [11,12] )
                  , (2, [   12,13] )
                  , (3, [] ) ]

  assertBool "4" $ runProgram d2
    [ (a, QFind $ findChildren $ Left 0)
    , (b, ( QQuant $ ForSome a a
              ( QQuant $ ForSome a2 a
                (QJunct $ QAnd
                 [ QVTest $ mkVTestCompare (<) (Right a) (Right a2)
                 , QFind $ findChildren $ Right a
                 , QFind $ findChildren $ Right a2 ] ) ) ) ) ]
    == Right
    ( M.fromList
      [ ( a, M.fromList [ ( 1, S.singleton M.empty)
                        , ( 2, S.singleton M.empty)
                        , ( 3, S.singleton M.empty) ] )
      , ( b, M.fromList [ ( 12, S.singleton $ M.fromList [(a,1),(a2,2)] ) ] )
      ] :: Possible Int )
