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


testModuleQueryClassify = TestList [
    TestLabel "test_findlike" test_findlike
  , TestLabel "test_introducesVars" test_introducesVars
  , TestLabel "test_usesVars" test_usesVars
--  , TestLabel "test_internalAndExternalVars" test_internalAndExternalVars
--  , TestLabel "test_disjointExistentials" test_disjointExistentials
--  , TestLabel "test_noPrematureReference" test_noPrematureReference
  -- these seem too easy to bother testing:
    -- validQuery
    -- feasible'Junctions
    -- usesOnlyIntroducedVars
  ]

type QIGI = Query Int (Graph Int)

--test_noPrematureReference = TestCase $ do
--  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
--      errMsg :: [Var] -> Either String ()
--      errMsg vs = Left $ "validProgram: variables " ++ show (S.fromList vs)
--                  ++ " used before being defined.\n"
--  assertBool "1" $ errMsg ["a","b"]
--    == noPrematureReference
--    [ ("a", QJunct $ And [ QFind $ findParents $ Right "a"
--                          , QFind $ findParents $ Right "b" ] :: QIGI ) ]
--  assertBool "2" $ errMsg ["a","b","d"]
--    == noPrematureReference
--    [ ("c", QJunct $ And [ QFind $ findParents $ Right "a"
--                          , QFind $ findParents $ Right "b" ] :: QIGI )
--    , ("d", QJunct $ And [ QFind $ findParents $ Right "c"
--                          , QFind $ findParents $ Right "d" ] ) ]
--  assertBool "3" $ Right ()
--    == noPrematureReference [ ("a", QFind $ findParents $ Left 3 :: QIGI)
--                            , ("b", QFind $ findParents $ Right "a") ]

-- COPY the block below to test multiple functions in Inspect.hs

--test_internalAndExternalVars = TestCase $ do
--  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
--      -- These queries are named for their internal and external variables.
--      c_ade, c_ag :: QIGI
--      c_ade = QQuant $ ForSome c d -- d was: (Source' d $ S.singleton a)
--        $ QFind $ findParents $ Right e
--      c_ag = QQuant $ ForSome c a -- a was: (Source' a $ S.singleton g)
--        $ QFind $ findParents $ Right c
--  assertBool "5" $ internalAndExternalVars
--    (QJunct $ Or [ c_ade, c_ag ] :: QIGI)
--    == (S.singleton c, S.fromList [a,d,e,g] )
--  assertBool "4" $ internalAndExternalVars c_ade
--    == (S.singleton c, S.fromList [a,d,e])
--  assertBool "3" $ internalAndExternalVars c_ag
--    == (S.singleton c, S.fromList [a,g])
--  assertBool "2" $ internalAndExternalVars
--    (QFind $ findParents $ Right c :: QIGI)
--    == (S.empty, S.singleton c)
--  assertBool "1" $ internalAndExternalVars
--    ( QQuant $ ForAll a b $ QJunct $ Or [ c_ade, c_ag ] )
--    == ( S.fromList [a,c ], S.fromList [b,d,e,g ] )

-- COPY the block above to test multiple functions in Inspect.hs

--test_disjointExistentials = TestCase $ do
--  let qf  = QFind $ Find (\_ _ -> S.empty) S.empty
--      [x,x1,x2,y,y1,y2] = ["x","x1","x2","y","y1","y2"]
--      qx  = QQuant $ ForSome  "x" x qf
--      qy  = QQuant $ ForSome  "y" y qf
--      qx1 = QQuant $ ForSome "x1" x qf
--      qy1 = QQuant $ ForSome "y1" y qf
--      qxy = QJunct $ Or [qx1,qy1]
--  assertBool "-1" $ disjointQuantifiers (QQuant $ ForSome x y qx)
--    == False
--  assertBool "0" $ disjointQuantifiers  (QQuant $ ForSome x x qy)
--    == False
--  assertBool "1" $ disjointQuantifiers  (QQuant $ ForSome x2 y qx1)
--    == True
--  assertBool "2" $ disjointQuantifiers  (QQuant $ ForSome y x qx1)
--    == True
--  assertBool "3" $ disjointQuantifiers qx               == False
--  assertBool "3.5" $ disjointQuantifiers qx1            == True
--  assertBool "3.7" $ disjointQuantifiers qxy            == True
--  assertBool "4" $ disjointQuantifiers (QJunct $ And [qx1,qxy]) == False

test_usesVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      -- These queries are named for their internal and external variables.
      c_ade, c_ag :: QIGI
      c_ade = QQuant $ ForSome c d -- d was: (Source' d $ S.singleton a)
        $ QFind $ findParents $ Right e
      c_ag = QQuant $ ForSome c a -- a was: (Source' a $ S.singleton g)
        $ QFind $ findParents $ Right c
  assertBool "5" $ usesVars
    (QJunct $ Or [ c_ade, c_ag ] :: QIGI)
    == S.fromList [c,e]
  assertBool "4" $ usesVars c_ade
    == S.singleton e
  assertBool "3" $ usesVars c_ag
    == S.singleton c
  assertBool "1" $ usesVars
    ( QQuant $ ( ForAll a b ( QJunct $ Or [ c_ade, c_ag ] )
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
