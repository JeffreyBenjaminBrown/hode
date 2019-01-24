{-# LANGUAGE ScopedTypeVariables #-}
module Test.TInspect where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Space.Graph
import Space.Graph.GQuery
import Query.Inspect
import Query.Leaf
import Types


type QIGI = Query Int (Graph Int)

testModuleQueryClassify = TestList [
    TestLabel "test_findlike" test_findlike
  , TestLabel "test_introducesVars" test_introducesVars
  , TestLabel "test_usesVars" test_usesVars
  , TestLabel "test_noAndCollisions" test_noAndCollisions
  , TestLabel "test_noIntroducedVarMasked" test_noIntroducedVarMasked
  , TestLabel "test_usesOnlyIntroducedVars" test_usesOnlyIntroducedVars
--  , TestLabel "test_noPrematureReference" test_noPrematureReference
  -- these seem too easy to bother testing:
    -- validQuery
    -- feasible'Junctions
  ]

--test_noPrematureReference = TestCase $ do
--  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
--      errMsg :: [Var] -> Either String ()
--      errMsg vs = Left $ "validProgram: variables " ++ show (S.fromList vs)
--                  ++ " used before being defined.\n"
--  assertBool "1" $ errMsg ["a","b"]
--    == noPrematureReference
--    [ ("a", QJunct $ And [ QFind $ findParents $ Right "a"
--                         , QFind $ findParents $ Right "b" ] :: QIGI ) ]
--  assertBool "2" $ errMsg ["a","b","d"]
--    == noPrematureReference
--    [ ("c", QJunct $ And [ QFind $ findParents $ Right "a"
--                          , QFind $ findParents $ Right "b" ] :: QIGI )
--    , ("d", QJunct $ And [ QFind $ findParents $ Right "c"
--                          , QFind $ findParents $ Right "d" ] ) ]
--  assertBool "3" $ Right ()
--    == noPrematureReference [ ("a", QFind $ findParents $ Left 3 :: QIGI)
--                            , ("b", QFind $ findParents $ Right "a") ]

test_usesOnlyIntroducedVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]

  assertBool "1" $ False
    == usesOnlyIntroducedVars
    ( QJunct $ And [ QFind $ findParents $ Right "a"
                   , QFind $ findParents $ Right "b" ] :: QIGI )
  assertBool "3" $ True
    == usesOnlyIntroducedVars
    ( QQuant $ ForSome "a1" "a" ( QFind $ findParents $ Right "a1" :: QIGI ) )
  assertBool "3" $ False
    == usesOnlyIntroducedVars
    ( QQuant $ ForSome "a1" "a" ( QFind $ findParents $ Right "b" :: QIGI ) )

test_noIntroducedVarMasked = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      [x,x1,x2,y,y1,y2] = ["x","x1","x2","y","y1","y2"]
      qx  = QQuant $ ForSome  "x" x qf
      qy  = QQuant $ ForSome  "y" y qf
      qx1 = QQuant $ ForSome "x1" x qf
      qy1 = QQuant $ ForSome "y1" y qf
      qxy = QJunct $ Or [qx1,qy1]
      t = noIntroducedVarMasked

  assertBool "-1"  $ t (QQuant $ ForSome x y qx)   == False
  assertBool "0"   $ t (QQuant $ ForSome x x qy)   == True
  assertBool "1"   $ t (QQuant $ ForSome x2 y qx1) == True
  assertBool "2"   $ t (QQuant $ ForSome y x qx1)  == True
  assertBool "3"   $ t qx                          == True
  assertBool "3.7" $ t qxy                         == True
  assertBool "4"   $ t (QJunct $ And [qx1,qxy])    == True


test_noAndCollisions = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
      [x,x1,x2,y,y1,y2,z] = ["x","x1","x2","y","y1","y2","z"]
      qx  = QQuant $ ForSome  x x qf
      qy  = QQuant $ ForSome  y y qf
      qx1 = QQuant $ ForSome x1 x qf
      qy1 = QQuant $ ForSome y1 y qf
      qxy = QJunct $ Or [qx1,qy1]

  assertBool "-1" $ noAndCollisions (QQuant $ ForSome x y qx)
    == True
  assertBool "0" $ noAndCollisions  (QQuant $ ForSome x x qy)
    == True

  assertBool "3" $ noAndCollisions qx               == True
  assertBool "3.5" $ noAndCollisions qx1            == True
  assertBool "3.7" $ noAndCollisions qxy            == True
  assertBool "4" $ noAndCollisions (QQuant $ ForSome z z
                                    $ QJunct $ And [qx1,qxy]) == False
  assertBool "4" $ noAndCollisions (QJunct $ And [qx1,qxy]) == False
  assertBool "4" $ noAndCollisions (QJunct $ Or [qx1,qxy]) == True

test_drawsFromVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      -- These queries are named for their internal and external variables.
      c_de, c_a :: QIGI
      c_de = QQuant $ ForSome c d -- d was: (Source' d $ S.singleton a)
        $ QFind $ findParents $ Right e
      c_a = QQuant $ ForSome c a -- a was: (Source' a $ S.singleton g)
        $ QFind $ findParents $ Right c

  assertBool "5" $ drawsFromVars (QJunct $ Or [ c_de, c_a ] :: QIGI)
                                      == S.fromList [d,a]
  assertBool "4" $ drawsFromVars c_de == S.singleton d
  assertBool "3" $ drawsFromVars c_a  == S.singleton a
  assertBool "2" $ drawsFromVars (QFind $ findParents $ Right c :: QIGI)
                                      == S.empty
  assertBool "1" $ drawsFromVars ( QQuant $ ForAll a b ( QJunct $ Or [ c_de, c_a ] ) [] )
                                      == S.fromList [b, d, a]

test_usesVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      -- These queries are named for their internal and external variables.
      c_de, c_a :: QIGI
      c_de = QQuant $ ForSome c d -- d was: (Source' d $ S.singleton a)
        $ QFind $ findParents $ Right e
      c_a = QQuant $ ForSome c a -- a was: (Source' a $ S.singleton g)
        $ QFind $ findParents $ Right c
  assertBool "5" $ usesVars
    (QJunct $ Or [ c_de, c_a ] :: QIGI)
    == S.fromList [c,e]
  assertBool "4" $ usesVars c_de
    == S.singleton e
  assertBool "3" $ usesVars c_a
    == S.singleton c
  assertBool "1" $ usesVars
    ( QQuant $ ( ForAll a b ( QJunct $ Or [ c_de, c_a ] )
                 [ mkVarTest (>) (Left 1) (Right f) ] ) )
    == S.fromList [c,e,f]

test_introducesVars = TestCase $ do
  let [a,b,c,x,y,z] = ["a","b","c","x","y","z"]
      q = QFind $ Find (\_ _ -> S.singleton 1) S.empty
      meh = error "whatever"
  assertBool "1" $ introducesVars (
    QJunct $ Or [ QQuant $ ForAll x x q []
                , QQuant $ ForAll y y
                  (QJunct $ And [ QQuant $ ForSome z z q ] )
                  [] ] )
    == S.fromList [x,y,z]

test_findlike = TestCase $ do
  let qf = QFind $ Find (\_ _    -> S.empty) S.empty
      qc = QTest $ Test (\_ _  _ -> False  ) S.empty
  assertBool "1" $ findlike (QJunct $ And [qf, qc]) == True
  assertBool "2" $ findlike (QJunct $ And [qf]    ) == True
  assertBool "3" $ findlike (QJunct $ And [qc]    ) == False
  assertBool "4" $ findlike (QJunct $ And []      ) == False
  assertBool "5" $ findlike (QJunct $ Or  [qf, qc]) == False
  assertBool "6" $ findlike (QJunct $ Or  [qf]    ) == True
  assertBool "7" $ findlike (QJunct $ Or  [qc]    ) == False
  assertBool "8" $ findlike (QJunct $ Or  []      ) == False
