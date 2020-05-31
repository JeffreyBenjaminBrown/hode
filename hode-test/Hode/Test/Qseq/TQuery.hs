{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Hode.Test.Qseq.TQuery where

import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Test.HUnit     as T
import           Test.HUnit hiding (Test, test)

import Hode.Data.Graph
import Hode.Qseq.Query
import Hode.Qseq.MkLeaf
import Hode.Qseq.Types


vs :: String -> Var
vs = VarString

test_module_query :: T.Test
test_module_query = TestList [
    TestLabel "test_runFindlike_Find" test_runFindlike_Find
  , TestLabel "test_runFindlike_ForSome" test_runFindlike_ForSome
  , TestLabel "test_runFindlike_ForAll" test_runFindlike_ForAll
  , TestLabel "test_runTestlike" test_runTestlike
  , TestLabel "testRunAnd" testRunAnd
  , TestLabel "test_runFindlike_mixed" test_runFindlike_mixed
  , TestLabel "test_runVarTestlike_complex" test_runVarTestlike_complex
  ]

test_runVarTestlike_complex :: T.Test
test_runVarTestlike_complex = TestCase $ do
  let [a,b,c] = map vs ["a","b","c"]
      [a1,b1,c1] = map vs ["a1","b1","c1"]
      (p :: Possible Int) = M.fromList
        [ (a, M.fromList [ (1, S.singleton M.empty)
                         , (2, S.singleton M.empty) ] )
        , (b, M.fromList [ (1, S.fromList [ M.singleton a 1
                                          , M.singleton a 2 ] )
                         , (2, S.fromList [ M.singleton a 1 ] )
                         , (3, S.fromList [ M.singleton a 1 ] ) ] )
        , (c, M.fromList [ (1, S.singleton M.empty)
                         , (2, S.fromList [ M.singleton b 2 ] ) ] ) ]

  assertBool "and" $
    substsThatPassVarTest (mkGraph [] :: Graph Int) p
    ( QJunct $ QAnd [ QVTest $ mkVTestIO' (a1,a) (b1,b)
                    , QVTest $ mkVTestIO' (b1,b) (c1,c) ] )
    ( [ M.fromList [ (a1,1), (b1,1), (c1,1) ]
      , M.fromList [ (a1,1), (b1,1), (c1,2) ]
      , M.fromList [ (a1,1), (b1,2), (c1,2) ]
      ] )
    == Right [ M.fromList [ (a1,1), (b1,2), (c1,2) ] ]

  assertBool "or" $
    substsThatPassVarTest (mkGraph [] :: Graph Int) p
    ( QJunct $ QOr [ QVTest $ mkVTestIO' (a1,a) (b1,b)
                   , QVTest $ mkVTestIO' (b1,b) (c1,c) ] )
    ( [ M.fromList [ (a1,1), (b1,3), (c1,0) ]
      , M.fromList [ (a1,0), (b1,1), (c1,2) ]
      , M.fromList [ (a1,0), (b1,2), (c1,2) ]
      ] )
    == Right [ M.fromList [ (a1,1), (b1,3), (c1,0) ]
             , M.fromList [ (a1,0), (b1,2), (c1,2) ] ]

  assertBool ( "\nA ForAll with a condition:\n Find all x s.t for all a0 in"
               ++ " {a | a0 > 10], x is a child of a0.\n" )
    $ let sp = mkGraph [ (  5, [])
                       , ( 15, [20,30,40    ] )
                       , ( 25, [   30,40, 50] ) ]
          (p4 :: Possible Int) =
            M.singleton (vs "a") $ M.fromList [ (5, S.singleton M.empty)
                                              , (15, S.singleton M.empty)
                                              , (25, S.singleton M.empty) ]
          q = QQuant $ ForAll (vs "a0") (vs "a")
              ( QVTest $ mkVTestCompare (<) (Left 10) $ Right (vs "a0") )
              $ QFind $ findChildren $ Right (vs "a0")
      in runFindlike sp p4 M.empty q
         == Right ( M.fromList [ (30, S.singleton M.empty )
                               , (40, S.singleton M.empty ) ] )

  assertBool ( "\nA varTestlike ForAll:\n"
             ++ "Find all a1 in a s.t. for all b(a1), b > 10.\n"
             ) $
    let sp = error "irrelevant"
        (p3 :: Possible Int) = M.fromList
            [ (a , M.fromList [ (1, S.singleton M.empty)
                              , (2, S.singleton M.empty) ] )
            , (b , M.fromList [ ( 5, S.singleton $ M.singleton a 1)
                              , (15, S.singleton $ M.singleton a 1)
                              , (15, S.singleton $ M.singleton b 2) ] ) ]
        q = QQuant $ ForSome a1 a -- a findlike QQuant
            $ QJunct $ QAnd
            [ QFind $ mkFindReturn $ Right a1
            , QQuant $ ForAll b1 b -- a varTestlike QQuant
              ( QVTest $ mkVTestIO' (a1,a) (b1,b) )
              $ QVTest $ mkVTestCompare (<) (Left 10) $ Right b1 ]
    in runFindlike sp p3 M.empty q
       == Right ( M.singleton 2 $ S.singleton $ M.singleton a1 2 )

  assertBool ( "\nA varTestlike ForSome:\n"
             ++ "Find all a1 in a s.t. for some b(a1), b > 10.\n"
             ) $
    let sp = error "irrelevant"
        (p2 :: Possible Int) = M.fromList
            [ (a , M.fromList [ (1, S.singleton M.empty)
                              , (2, S.singleton M.empty)
                              , (3, S.singleton M.empty) ] )
            , (b , M.fromList [ ( 5, S.fromList [ M.singleton a 1
                                                 , M.singleton a 3 ] )
                              , (15, S.fromList  [ M.singleton a 1
                                                 , M.singleton a 2 ] )
                              , (25, S.fromList  [ M.singleton a 2 ] ) ] ) ]
        q = QQuant $ ForSome a1 a -- a findlike QQuant
            $ QJunct $ QAnd
            [ QFind $ mkFindReturn $ Right a1
            , QQuant $ ForSome b1 b -- a varTestlike QQuant
              $ QJunct $ QAnd
              [ QVTest $ mkVTestIO' (a1,a) (b1,b)
              , QVTest $ mkVTestCompare (<) (Left 10) $ Right b1
              ] ]
    in runFindlike sp p2 M.empty q
    == Right ( M.fromList [ (1, S.fromList [ M.fromList [(a1,1)] ] )
                          , (2, S.fromList [ M.fromList [(a1,2)] ] )
                          ] )

test_runFindlike_mixed :: T.Test
test_runFindlike_mixed = TestCase $ do
  let [a,b]   = map vs ["a","b"]
      [a1,b1] = map vs ["a1","b1"]
      [a2]    = [(vs "a2")]
      isnt v = QTest $ mkTest (/=) $ Right v
      d = mkGraph [ (0, [1,2        ] )
                  , (3, [  2,3,4    ] )
                  , (10,[11, 23     ] ) ]
      (p :: (Possible Int)) = M.fromList
        [ (a, M.fromList $ map (, S.singleton M.empty) [1,2,3])
        , (b, M.fromList [ (11, S.singleton $ M.singleton a 1)
                         , (23, S.fromList [ M.singleton a 2
                                           , M.singleton a 3 ] ) ] ) ]
      fc3 = QFind $ findChildren $ Left 3
      fc v = QFind $ findChildren $ Right v
      s = M.fromList [(a1,2), (b1,23)] :: (Subst Int)

  assertBool "2" $ runFindlike d p s
        ( QJunct $ QAnd
             [ QQuant $ ForAll a2 a
               ( QVTest $ mkVTestIO' (a2,a) (b1,b) )
               $ isnt a2
             , QQuant $ ForSome a1 a $ QJunct $ QAnd
               [ fc a1
               , QVTest $ mkVTestIO' (a1,a) (b1,b) ] ] )
    == Right ( M.singleton 4 (S.fromList [ M.singleton a1 3 ] ) )

  assertBool "1" $ runFindlike d p s
    ( QQuant ( ForAll a1 a (QJunct $ QAnd [])
               ( QJunct $ QAnd [ fc3, isnt a1 ] ) ) )
    == Right ( M.singleton 4 $ S.singleton $ M.empty )

testRunAnd :: T.Test
testRunAnd = TestCase $ do
  let [a] = map vs ["a"]
      nota = QTest $ mkTest (/=) $ Right a
      d = mkGraph [ (0, [1,2    ] )
                  , (3, [  2,3,4] ) ]
      (p :: (Possible Int)) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (3, S.singleton M.empty) ]
      fc0 = QFind $ findChildren $ Left 0
  assertBool "1" $ ( runAnd d p (M.singleton a 2 :: Subst Int)
                     [fc0, nota] )
    == Right ( M.singleton 1 (S.singleton $ M.singleton a 2) )
  assertBool "2" $ ( runAnd d p (M.singleton a 1 :: Subst Int)
                     [fc0, nota] )
    == Right ( M.singleton 2 (S.singleton $ M.singleton a 1) )

test_runTestlike :: T.Test
test_runTestlike = TestCase $ do
  let [a,x] = map vs ["a","x"]
      [a1]  = map vs ["a1"]
      (a2 :: (Subst Int)) = M.singleton a 2
      (not3 :: Test Int (Graph Int)) = mkTest (/=) $ Left 3
      (nota :: Test Int (Graph Int)) = mkTest (/=) $ Right a
      (nota1 :: Test Int (Graph Int)) = mkTest (/=) $ Right a1
      d = mkGraph $ map (,[]) [1..3]
      (p :: (Possible Int)) = M.singleton a $
        M.fromList [ (1, S.singleton M.empty)
                   , (2, S.singleton M.empty) ]
      (ce :: (CondElts Int)) = M.fromList [ (1, S.singleton $ M.singleton x 0)
                                          , (2, S.singleton M.empty)
                                          , (3, S.singleton M.empty) ]

  assertBool "6" $ runTestlike d p ce (M.empty :: Subst Int)
    ( QQuant $ ( ForAll a1 a
                 ( QVTest $ mkVTestCompare (<) (Left 1) (Right a1) )
                 $ QTest nota1 ) )
    == Right ( M.fromList [ (1, S.singleton $ M.singleton x 0)
                          , (3, S.singleton M.empty) ] )

  assertBool "5" $ runTestlike d p ce (M.empty :: Subst Int)
    ( QQuant $ ForAll a1 a (QJunct $ QAnd []) $ QTest nota1 )
    == Right ( M.singleton 3 (S.singleton M.empty) )

  assertBool "4" $ let
    qfa = QQuant $ ForSome a1 a
          $ QJunct $ QAnd [QTest not3, QTest nota1]
    ce23 = M.restrictKeys ce $ S.singleton 2
    in runTestlike d p ce23 (M.empty :: Subst Int) qfa
       == Right ( M.singleton 2 (S.singleton $ M.singleton a1 1) )

  assertBool "3" $ runTestlike d (M.empty :: Possible Int) ce a2
    (QJunct $ QOr [QTest not3, QTest nota])
    == Right ( M.fromList
               [ (1, S.fromList [ M.fromList [(a,2), (x,0)]
                                , M.singleton x 0           ] )
               , (2, S.fromList [ M.empty                   ] )
               , (3, S.fromList [          M.singleton a 2  ] ) ] )

  assertBool "2" $ runTestlike d (M.empty :: Possible Int) ce a2
    (QJunct $ QAnd [QTest not3, QTest nota])
    == Right ( M.singleton 1 $ S.singleton $ M.fromList [(a,2), (x,0)] )

  assertBool "1" $ runTestlike d M.empty ce a2 (QTest nota)
    == Right ( M.fromList [ (1, S.singleton $ M.fromList [(a,2), (x,0)] )
                          , (3, S.singleton $ M.singleton a 2) ] )

test_runFindlike_ForAll :: T.Test
test_runFindlike_ForAll = TestCase $ do
  let g = mkGraph [ (1, [11, 12    ] )
                  , (2, [    12, 22] ) ]
      [a,b,c,x]  = map vs ["a","b","c","x"]
      [a1,b1,c1] = map vs ["a1","b1","c1"]
      (p :: (Possible Int)) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton $ M.singleton x 1) ] )
          , ( c, M.fromList [ (1, S.singleton $ M.singleton a 2) ] ) ]
      qc :: Var -> (Query Int (Graph Int))
      qc v = QFind $ findChildren $ Right v

  assertBool "4" $ runFindlike g p
    (M.singleton c1 1)
    ( QQuant ( ForAll a1 a
               ( QVTest $ mkVTestIO' (a1,a) (c1,c) )
               $ qc a1 ) )
    == Right ( M.fromList [ (12, S.singleton $ M.empty)
                          , (22, S.singleton $ M.empty) ] )

  assertBool "3" $ runFindlike g p
    ( M.singleton x 1 )
    ( QQuant $ ForAll b1 b (QJunct $ QAnd []) $ qc b1 )
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

  assertBool "3.5" $
    runFindlike g p
    ( M.singleton x 1 )
    ( QQuant $ ForAll b1 b (QJunct $ QAnd []) $ qc b1 )
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

  assertBool "2" $ runFindlike g p M.empty
    ( QQuant $ ForAll b1 b (QJunct $ QAnd []) $ qc b1 )
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

  assertBool "1" $ runFindlike g p M.empty
    ( QQuant $ ForAll a1 a (QJunct $ QAnd []) $ qc a1 )
    == Right ( M.fromList [ (12, S.singleton M.empty) ] )

test_runFindlike_ForSome :: T.Test
test_runFindlike_ForSome = TestCase $ do
  let g = mkGraph [ (1, [11, 21] )
                  , (2, [12, 22] ) ]
      [a,b]   = map vs ["a","b"]
      [a1,b1] = map vs ["a1","b1"]
      (p:: Possible Int) = M.fromList
          [ ( a, M.fromList [ (1, S.singleton   M.empty)
                            , (2, S.singleton   M.empty) ] )
          , ( b, M.fromList [ (1, S.singleton $ M.singleton a 1)
                            , (2, S.singleton   M.empty) ] ) ]
      qc :: Var -> (Query Int (Graph Int))
      qc v = QFind $ findChildren $ Right v

  assertBool "3" $ ( runFindlike g p
                     ( M.singleton b1 1 )
                     $ QQuant $ ForSome a1 a $ QJunct
                     $ QAnd [ QVTest $ mkVTestIO' (a1,a) (b1,b)
                           , qc a1 ] )
    == Right (M.fromList [ (11, S.singleton $ M.fromList [ (a1, 1) ] )
                         , (21, S.singleton $ M.fromList [ (a1, 1) ] ) ] )

  assertBool "2" $ ( runFindlike g p
                     ( M.singleton b1 2 ) -- the difference
                     $ QQuant $ ForSome a1 a $ QJunct
                     $ QAnd [ QVTest $ mkVTestIO' (a1,a) (b1,b)
                           , qc a1 ] )
    == Right M.empty

  assertBool "1" $ ( runFindlike g p M.empty
                     $ QQuant $ ForSome a1 a $ qc a1)
    == Right ( M.fromList [ (11, S.singleton $ M.singleton a1 1)
                          , (21, S.singleton $ M.singleton a1 1)
                          , (12, S.singleton $ M.singleton a1 2)
                          , (22, S.singleton $ M.singleton a1 2) ] )

test_runFindlike_Find :: T.Test
test_runFindlike_Find = TestCase $ do
  let g = mkGraph [ (1, [11, 21] )
                  , (2, [12, 22] ) ]
      [x,y] = map vs ["x","y"]
      f1 = QFind $ findChildren $ Left 1
      fy = QFind $ findChildren $ Right y
      s = M.fromList [(x,1), (y,2)] :: Subst Int
  assertBool "1" $ runFindlike g M.empty M.empty f1
    == Right ( M.fromList [ (11, S.singleton M.empty)
                  , (21, S.singleton M.empty) ] )
  assertBool "2" $ runFindlike g M.empty s fy
    == Right ( M.fromList [ (12, S.singleton $ M.singleton y 2)
                          , (22, S.singleton $ M.singleton y 2) ] )
