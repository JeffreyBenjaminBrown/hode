{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Test.TQuery where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Space.Graph
import Query
import Query.Inspect
import Types


testModuleQuery = TestList [
    TestLabel "test_runFindlike_Find" test_runFindlike_Find
  , TestLabel "testRunTest" testRunTest
  , TestLabel "test_runFindlike_ForSome" test_runFindlike_ForSome
  , TestLabel "test_runFindlike_ForAll" test_runFindlike_ForAll
  , TestLabel "testRunTestlike" testRunTestlike
  , TestLabel "testRunAnd" testRunAnd
  , TestLabel "test_runFindlike_mixed" test_runFindlike_mixed
  , TestLabel "test_isNot_2" test_isNot_2
  ]

test_isNot_2 = TestCase $ do
  let (a,b) = ("a","b")
      t_34, t_ab :: VarTest Int (Graph Int)
      t_34 = mkVarTest (/=) (Left 3)    (Left 4)
      t_ab = mkVarTest (/=) (Right "a") (Right "b")
      a1b1 = M.fromList [(a,1),(b,1)]
      a1b2 = M.fromList [(a,1),(b,2)]
  assertBool "1" $ True  == varTestFunction t_34 (graph []) M.empty
  assertBool "2" $ False == varTestFunction t_ab (graph []) a1b1
  assertBool "3" $ True  == varTestFunction t_ab (graph []) a1b2

test_runFindlike_mixed = TestCase $ do
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      aOf_b = "aOf_b"
      src_aOf_b = Source' a $ S.singleton b

      isnt v = QTest $ mkTest (/=) $ Right v
      d = graph [ (0, [1,2        ] )
                , (3, [  2,3,4    ] )
                , (10,[11, 23     ] ) ]
      (p :: (Possible Int)) = M.fromList
        [ (a, M.fromList $ map (, S.singleton M.empty) [1,2,3])
        , (b, M.fromList [ (11, S.singleton $ M.singleton a 1)
                         , (23, S.fromList [ M.singleton a 2
                                           , M.singleton a 3 ] ) ] ) ]
      fc0 = QFind $ findChildren $ Left 0
      fc v = QFind $ findChildren $ Right v
      s = M.fromList [(a,2), (b,23)] :: (Subst Int)
      q_And_Quant = let aOf_b' = aOf_b ++ "'"
        in QJunct $ And [ QQuant $ ForAll aOf_b' src_aOf_b $ isnt aOf_b'
                         , QQuant $ ForSome aOf_b src_aOf_b $ fc aOf_b ]
      q_ForAll_And = QQuant $ ForAll aOf_b src_aOf_b
                     $ QJunct $ And [ fc0, isnt a ]

  assertBool "2" $ runFindlike d p q_And_Quant s
    == Right ( M.singleton 4 (S.fromList [ M.singleton aOf_b 3 ] ) )
  assertBool "1" $ runFindlike d p q_ForAll_And s
    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )

testRunAnd = TestCase $ do
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      (a2 :: (Subst Int)) = M.singleton a 2
      nota = QTest $ mkTest (/=) $ Right a
      d = graph [ (0, [1,2    ] )
                , (3, [  2,3,4] ) ]
      (p :: (Possible Int)) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (3, S.singleton M.empty) ]
      fc0 = QFind $ findChildren $ Left 0
  assertBool "1" $ runAnd d p [fc0, nota] a2
    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )

testRunTestlike = TestCase $ do
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      (a2 :: (Subst Int)) = M.singleton a 2
      (not3 :: Test Int (Graph Int)) = mkTest (/=) $ Left 3
      (nota :: Test Int (Graph Int)) = mkTest (/=) $ Right a
      d = graph $ map (,[]) [1..3]
      (p :: (Possible Int)) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (2, S.singleton M.empty) ]
      (ce :: (CondElts Int)) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                    , (2, S.singleton M.empty)
                                    , (3, S.singleton M.empty) ]
  assertBool "5" $ runTestlike d p
    (QQuant $ ForAll a (Source a) $ QTest nota) M.empty ce
    == Right ( M.singleton 3 (S.singleton M.empty) )
  assertBool "4" $ let
    qfa = QQuant $ ForSome a (Source a) $ QJunct $ And [QTest not3, QTest nota]
    ce23 = M.restrictKeys ce $ S.singleton 2
    in runTestlike d p qfa M.empty ce23
       == Right ( M.singleton 2 (S.singleton $ M.singleton a 1) )
  assertBool "3" $ runTestlike d M.empty
    (QJunct $ Or  [QTest not3,QTest nota]) a2 ce
    == Right ( M.fromList
               [ (1, S.fromList [ M.fromList [(a,2), (x,0)]
                                , M.singleton x 0           ] )
               , (2, S.fromList [ M.empty                   ] )
               , (3, S.fromList [          M.singleton a 2  ] ) ] )
  assertBool "2" $ runTestlike d M.empty
    (QJunct $ And [QTest not3,QTest nota]) a2 ce
    == Right ( M.singleton 1 $ S.singleton $ M.fromList [(a,2), (x,0)] )
  assertBool "1" $ runTestlike d M.empty (QTest nota)                   a2 ce
    == Right ( M.fromList [ (1, S.singleton $ M.fromList [(a,2), (x,0)] )
                          , (3, S.singleton $ M.singleton a 2) ] )

test_runFindlike_ForAll = TestCase $ do
  let g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      [a,b,c,x,y] = ["a","b","c","x","y"]
      aOf_c = "aOf_c"
      src_aOf_c = Source' a $ S.singleton c
      (p :: (Possible Int)) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton $ M.singleton x 1) ] )
          , ( c, M.fromList [ (1, S.singleton $ M.singleton a 2) ] ) ]
      qc :: Var -> (Query Int (Graph Int))
      qc v = QFind $ findChildren $ Right v

  assertBool "4" $ runFindlike g p
    (QQuant $ ForAll aOf_c src_aOf_c $ qc aOf_c) (M.singleton c 1)
    == Right ( M.fromList [ (12, S.singleton $ M.empty)
                          , (22, S.singleton $ M.empty) ] )
  assertBool "3" $ runFindlike g p
    (QQuant $ ForAll b (Source b) $ qc b) (M.singleton x 1)
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
  assertBool "3" $
    runFindlike g p
    (QQuant $ ForAll b (Source b) $ qc b) (M.singleton x 1)
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
  assertBool "2" $ runFindlike g p
    (QQuant $ ForAll b (Source b) $ qc b) M.empty
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
  assertBool "1" $ runFindlike g p
    (QQuant $ ForAll a (Source a) $ qc a) M.empty
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

test_runFindlike_ForSome = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [a,b,x,y] = ["a","b","x","y"]
      aOf_b = "aOf_b"
      src_aOf_b = Source' a $ S.singleton b
      (p:: Possible Int) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton $ M.singleton a 1)
                            , (2, S.singleton   M.empty) ] ) ]
      qc :: Var -> (Query Int (Graph Int))
      qc v = QFind $ findChildren $ Right v

  assertBool "3" $ ( runFindlike g p
                    (QQuant $ ForSome aOf_b src_aOf_b $ qc aOf_b)
                    $ M.singleton b 1 )
    == Right (M.fromList [ (11, S.singleton $ M.fromList [ (aOf_b, 1) ] )
                         , (21, S.singleton $ M.fromList [ (aOf_b, 1) ] ) ] )
  assertBool "2" $ ( runFindlike g p
                     (QQuant $ ForSome aOf_b src_aOf_b $ qc aOf_b)
                     $ M.singleton b 2)
    == Right M.empty
  assertBool "1" $
    runFindlike g p (QQuant $ ForSome a (Source a) $ qc a) M.empty
    == Right ( M.fromList [ (11, S.singleton $ M.singleton a 1)
                          , (21, S.singleton $ M.singleton a 1)
                          , (12, S.singleton $ M.singleton a 2)
                          , (22, S.singleton $ M.singleton a 2) ] )

testRunTest = TestCase $ do
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      g = graph [ (1, [11, 12    ] )
                , (2, [    12, 22] ) ]
      (a2 :: (Subst Int)) = M.singleton a 2
      (ce :: (CondElts Int)) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                    , (2, S.singleton $ M.empty) ]
  assertBool "1" $ runTest g a2 (mkTest (/=) $ Left 1) ce
    == M.singleton 2 (S.singleton M.empty)
  assertBool "2" $ runTest g a2 (mkTest (/=) $ Right a) ce
    == M.singleton 1 ( S.singleton $ M.fromList [(a,2), (x,0)] )

test_runFindlike_Find = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [x,y] = ["x","y"]
      f1 = QFind $ findChildren $ Left 1
      fy = QFind $ findChildren $ Right y
      s = M.fromList [(x,1), (y,2)] :: Subst Int
  assertBool "1" $ runFindlike g M.empty f1 M.empty
    == Right ( M.fromList [ (11, S.singleton M.empty)
                  , (21, S.singleton M.empty) ] )
  assertBool "2" $ runFindlike g M.empty fy s
    == Right ( M.fromList [ (12, S.singleton $ M.singleton y 2)
                          , (22, S.singleton $ M.singleton y 2) ] )
