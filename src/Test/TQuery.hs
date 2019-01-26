{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Test.TQuery where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test, test)

import Space.Graph
import Space.Graph.GQuery
import Query
import Query.Inspect
import Query.MkLeaf
import Query.RunLeaf
import Types


test_module_query = TestList [
    TestLabel "test_runFindlike_Find" test_runFindlike_Find
  , TestLabel "test_runFindlike_ForSome" test_runFindlike_ForSome
--  , TestLabel "test_runFindlike_ForAll" test_runFindlike_ForAll
  , TestLabel "test_runTestlike" test_runTestlike
--  , TestLabel "testRunAnd" testRunAnd
--  , TestLabel "test_runFindlike_mixed" test_runFindlike_mixed
  ]

--test_runFindlike_mixed = TestCase $ do
--  let [a,b,c,x,y] = ["a","b","c","x","y"]
--      aOf_b = "aOf_b"
--      src_aOf_b = Source' a $ S.singleton b
--
--      isnt v = QTest $ test (/=) $ Right v
--      d = graph [ (0, [1,2        ] )
--                , (3, [  2,3,4    ] )
--                , (10,[11, 23     ] ) ]
--      (p :: (Possible Int)) = M.fromList
--        [ (a, M.fromList $ map (, S.singleton M.empty) [1,2,3])
--        , (b, M.fromList [ (11, S.singleton $ M.singleton a 1)
--                         , (23, S.fromList [ M.singleton a 2
--                                           , M.singleton a 3 ] ) ] ) ]
--      fc0 = QFind $ findChildren $ Left 0
--      fc v = QFind $ findChildren $ Right v
--      s = M.fromList [(a,2), (b,23)] :: (Subst Int)
--      q_And_Quant = let aOf_b' = aOf_b ++ "'"
--        in QJunct $ And
--           [ QQuant $ ForAll aOf_b' src_aOf_b $ isnt aOf_b' []
--           , QQuant $ ForSome aOf_b src_aOf_b $ fc aOf_b ]
--      q_ForAll_And = QQuant $ ForAll aOf_b src_aOf_b
--                     ( QJunct $ And [ fc0, isnt a ] )
--                     []
--
--  assertBool "2" $ runFindlike d p q_And_Quant s
--    == Right ( M.singleton 4 (S.fromList [ M.singleton aOf_b 3 ] ) )
--  assertBool "1" $ runFindlike d p q_ForAll_And s
--    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )

--testRunAnd = TestCase $ do
--  let [a,b,c,x,y] = ["a","b","c","x","y"]
--      (a2 :: (Subst Int)) = M.singleton a 2
--      nota = QTest $ test (/=) $ Right a
--      d = graph [ (0, [1,2    ] )
--                , (3, [  2,3,4] ) ]
--      (p :: (Possible Int)) = M.singleton a $
--        M.fromList [ (1, S.singleton M.empty)
--                   , (3, S.singleton M.empty) ]
--      fc0 = QFind $ findChildren $ Left 0
--  assertBool "1" $ runAnd d p [fc0, nota] a2
--    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )

test_runTestlike = TestCase $ do
  let [a,b,c,x,y] = ["a","b","c","x","y"]
      [a1,b1,c1,x1,y1] = ["a1","b1","c1","x1","y1"]
      (a2 :: (Subst Int)) = M.singleton a 2
      (not3 :: Test Int (Graph Int)) = test (/=) $ Left 3
      (nota :: Test Int (Graph Int)) = test (/=) $ Right a
      (nota1 :: Test Int (Graph Int)) = test (/=) $ Right a1
      d = graph $ map (,[]) [1..3]
      (p :: (Possible Int)) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (2, S.singleton M.empty) ]
      (ce :: (CondElts Int)) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                          , (2, S.singleton M.empty)
                                          , (3, S.singleton M.empty) ]

  assertBool "6" $ runTestlike d p ce (M.empty :: Subst Int)
    ( QQuant $ ForAll a1 a (QTest nota1)
      [ varTestCompare (<) (Left 1) (Right a1) ] )
    == Right ( M.fromList [ (1, S.singleton $ M.singleton x 0)
                          , (3, S.singleton M.empty) ] )

  assertBool "5" $ runTestlike d p ce (M.empty :: Subst Int)
    (QQuant $ ForAll a1 a (QTest nota1) [])
    == Right ( M.singleton 3 (S.singleton M.empty) )

  assertBool "4" $ let
    qfa = QQuant $ ForSome a1 a
          $ QJunct $ And [QTest not3, QTest nota1]
    ce23 = M.restrictKeys ce $ S.singleton 2
    in runTestlike d p ce23 (M.empty :: Subst Int) qfa
       == Right ( M.singleton 2 (S.singleton $ M.singleton a1 1) )

  assertBool "3" $ runTestlike d (M.empty :: Possible Int) ce a2
    (QJunct $ Or [QTest not3, QTest nota])
    == Right ( M.fromList
               [ (1, S.fromList [ M.fromList [(a,2), (x,0)]
                                , M.singleton x 0           ] )
               , (2, S.fromList [ M.empty                   ] )
               , (3, S.fromList [          M.singleton a 2  ] ) ] )

  assertBool "2" $ runTestlike d (M.empty :: Possible Int) ce a2
    (QJunct $ And [QTest not3, QTest nota])
    == Right ( M.singleton 1 $ S.singleton $ M.fromList [(a,2), (x,0)] )

  assertBool "1" $ runTestlike d M.empty ce a2 (QTest nota)
    == Right ( M.fromList [ (1, S.singleton $ M.fromList [(a,2), (x,0)] )
                          , (3, S.singleton $ M.singleton a 2) ] )

--test_runFindlike_ForAll = TestCase $ do
--  let g = graph [ (1, [11, 12    ] )
--                , (2, [    12, 22] ) ]
--      [a,b,c,x,y] = ["a","b","c","x","y"]
--      aOf_c = "aOf_c"
--      src_aOf_c = Source' a $ S.singleton c
--      (p :: (Possible Int)) = M.fromList
--          [ ( a, M.fromList [ (1, S.singleton   M.empty)
--                            , (2, S.singleton   M.empty) ] )
--          , ( b, M.fromList [ (1, S.singleton   M.empty)
--                            , (2, S.singleton $ M.singleton x 1) ] )
--          , ( c, M.fromList [ (1, S.singleton $ M.singleton a 2) ] ) ]
--      qc :: Var -> (Query Int (Graph Int))
--      qc v = QFind $ findChildren $ Right v
--
--  assertBool "4" $ runFindlike g p
--    (QQuant $ ForAll aOf_c src_aOf_c $ qc aOf_c) (M.singleton c 1)
--    == Right ( M.fromList [ (12, S.singleton $ M.empty)
--                          , (22, S.singleton $ M.empty) ] )
--  assertBool "3" $ runFindlike g p
--    (QQuant $ ForAll b (Source b) $ qc b) (M.singleton x 1)
--    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
--  assertBool "3" $
--    runFindlike g p
--    (QQuant $ ForAll b (Source b) $ qc b) (M.singleton x 1)
--    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
--  assertBool "2" $ runFindlike g p
--    (QQuant $ ForAll b (Source b) $ qc b) M.empty
--    == Right ( M.fromList [ (12, S.singleton M.empty) ] )
--  assertBool "1" $ runFindlike g p
--    (QQuant $ ForAll a (Source a) $ qc a) M.empty
--    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

test_runFindlike_ForSome = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [a,b,x,y] = ["a","b","x","y"]
      [a1,b1,x1,y1] = ["a1","b1","x1","y1"]
      --aOf_b = "aOf_b"
      --src_aOf_b = Source' a $ S.singleton b
      (p:: Possible Int) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton $ M.singleton a 1)
                            , (2, S.singleton   M.empty) ] ) ]
      qc :: Var -> (Query Int (Graph Int))
      qc v = QFind $ findChildren $ Right v

  assertBool "3" $ ( runFindlike g p
                     ( M.singleton b1 1 )
                     -- $ QQuant $ ForSome aOf_b src_aOf_b $ qc aOf_b
                     $ QQuant $ ForSome a1 a $ QJunct
                     $ And [ QVTest $ varTestIO' (a1,a) (b1,b)
                           , qc a1 ] )
    == Right (M.fromList [ (11, S.singleton $ M.fromList [ (a1, 1) ] )
                         , (21, S.singleton $ M.fromList [ (a1, 1) ] ) ] )

  assertBool "2" $ ( runFindlike g p
                     ( M.singleton b1 2 ) -- the difference
                     -- $ QQuant $ ForSome aOf_b src_aOf_b $ qc aOf_b
                     $ QQuant $ ForSome a1 a $ QJunct
                     $ And [ QVTest $ varTestIO' (a1,a) (b1,b)
                           , qc a1 ] )
    == Right M.empty

  assertBool "1" $ ( runFindlike g p M.empty
                     $ QQuant $ ForSome a1 a $ qc a1)
    == Right ( M.fromList [ (11, S.singleton $ M.singleton a1 1)
                          , (21, S.singleton $ M.singleton a1 1)
                          , (12, S.singleton $ M.singleton a1 2)
                          , (22, S.singleton $ M.singleton a1 2) ] )

test_runFindlike_Find = TestCase $ do
  let g = graph [ (1, [11, 21] )
                , (2, [12, 22] ) ]
      [x,y] = ["x","y"]
      f1 = QFind $ findChildren $ Left 1
      fy = QFind $ findChildren $ Right y
      s = M.fromList [(x,1), (y,2)] :: Subst Int
  assertBool "1" $ runFindlike g M.empty M.empty f1
    == Right ( M.fromList [ (11, S.singleton M.empty)
                  , (21, S.singleton M.empty) ] )
  assertBool "2" $ runFindlike g M.empty s fy
    == Right ( M.fromList [ (12, S.singleton $ M.singleton y 2)
                          , (22, S.singleton $ M.singleton y 2) ] )
