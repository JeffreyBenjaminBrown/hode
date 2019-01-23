{-# LANGUAGE ScopedTypeVariables #-}
module Test.TProgram where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Space.Graph
import Program
import Query
import Types


test_module_Program = TestList [
  TestLabel "test_runProgram" test_runProgram
  ]

test_runProgram = TestCase $ do
  let [a,b,c,e,f,g,h,x,y,z] = ["a","b","c","e","f","g","h","x","y","z"]
      [a1,b1,c1,e1,f1,g1,h1,x1,y1,z1] =
        ["a1","b1","c1","e1","f1","g1","h1","x1","y1","z1"]
      [a2,b2,c2,e2,f2,g2,h2,x2,y2,z2] =
        ["a2","b2","c2","e2","f2","g2","h2","x2","y2","z2"]
      d = graph [ (0, [1,2        ] )
                , (3, [  2,3,4    ] )
                , (10,[11, 23     ] ) ]

  assertBool "1" $ runProgram d [ (a, QFind $ findParents $ Left 2) ]
    == Right ( M.singleton a ( M.fromList [ (0,S.singleton M.empty)
                                            , (3,S.singleton M.empty) ] ) )

  assertBool "2" $ runProgram d
    [ ( b, ( QJunct $ And [ QFind $ findChildren $ Left 3
                             , QTest $ mkTest (/=) $ Left 2 ] ) ) ]
    == Right (M.singleton b ( M.fromList [ (3, S.singleton M.empty)
                                           , (4, S.singleton M.empty) ] ) )

  assertBool "3" $ runProgram d
    [ ( a, QFind $ findParents $ Left 2)
    , ( b, ( QQuant $ ForSome a1 a $
               QJunct $ And
               [ QFind $  mkFindFrom "children" children $ Right a1
               , QTest $ mkTest (/=) $ Right a1
               , QTest $ mkTest (/=) $ Left 2 ] ) ) ]
    == Right ( M.fromList
               [ ( a, M.fromList [ (0, S.singleton M.empty)
                                   , (3, S.singleton M.empty) ] )
               , ( b, M.fromList [ (1, S.singleton $ M.singleton a1 0)
                                   , (4, S.singleton $ M.singleton a1 3)
                                   ] ) ] )

  let d = graph [ (0, [1,2,3] )
                , (1, [11,12] )
                , (2, [   12,13] )
                , (3, [] ) ]

  assertBool "4" $ runProgram d
    [ (a, QFind $ findChildren $ Left 0)
    , (b, ( QQuant $ ForSome a1 a
              ( QQuant $ ForSome a2 a
                (QJunct $ And
                 [ QVTest $ mkVarTest (<) (Right a1) (Right a2)
                 , QFind $ findChildren $ Right a1
                 , QFind $ findChildren $ Right a2 ] ) ) ) ) ]
    == Right ( M.fromList
               [ ( a, M.fromList [ (1, S.singleton M.empty)
                                   , (2, S.singleton M.empty)
                                   , (3, S.singleton M.empty) ] )
               , ( b, M.fromList [ (12, ( S.singleton
                                            $ M.fromList [(a1,1),(a2,2)]
                                          ) ) ] ) ] )
