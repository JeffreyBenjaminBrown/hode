{-# LANGUAGE ScopedTypeVariables #-}
module Test.Qseq.TValid where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import Data.Graph
import Qseq.QValid
import Qseq.MkLeaf
import Qseq.QTypes


type QIGI = Query Int (Graph Int)

testModuleQueryClassify = TestList [
    TestLabel "test_findlike" test_findlike
  , TestLabel "test_introducesVars" test_introducesVars
  , TestLabel "test_usesVars" test_usesVars
  , TestLabel "test_noAndCollisions" test_noAndCollisions
  , TestLabel "test_noIntroducedVarMasked" test_noIntroducedVarMasked
  , TestLabel "test_usesOnlyIntroducedVars" test_usesOnlyIntroducedVars
  , TestLabel "test_usesNoSourceBeforeItExists"
    test_usesNoSourceBeforeItExists
  -- these seem too easy to bother testing:
    -- validQuery
    -- feasibleJunctions
  ]

test_usesNoSourceBeforeItExists = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      errMsg :: [Var] -> Either String ()
      errMsg vs = Left $ "validProgram: variables " ++ show (S.fromList vs)
                  ++ " used before being defined.\n"
  assertBool "no sources. (use is different from sourcing.)" $ Right ()
    == usesNoSourceBeforeItExists
    [ ("c", QJunct $ QAnd [ QFind $ findParents $ Right "a"
                          , QFind $ findParents $ Right "b" ] :: QIGI )
    , ("d", QJunct $ QAnd [ QFind $ findParents $ Right "c"
                          , QFind $ findParents $ Right "d" ] ) ]
  assertBool "3" $ Right ()
    == usesNoSourceBeforeItExists
    [ ("a", QFind $ findParents $ Left 3 :: QIGI)
    , ("b", QQuant $ ForSome "a1" "a" $ QFind $ findParents $ Right "a") ]
  assertBool "3" $ Left (
    "validProgram: variables used before being defined: "
    ++ show (S.singleton "x") ++ ".\n" )
    == usesNoSourceBeforeItExists
    [ ("a", QFind $ findParents $ Left 3 :: QIGI)
    , ("b", QQuant $ ForSome "y" "x" $ QFind $ findParents $ Right "a") ]

test_usesOnlyIntroducedVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]

  assertBool "1" $ isLeft $ usesOnlyIntroducedVars
    ( QJunct $ QAnd [ QFind $ findParents $ Right "a"
                    , QFind $ findParents $ Right "b" ] :: QIGI )
  assertBool "3" $ isRight $ usesOnlyIntroducedVars
    ( QQuant $ ForSome "a1" "a" ( QFind $ findParents $ Right "a1" :: QIGI ) )
  assertBool "3" $ isLeft $ usesOnlyIntroducedVars
    ( QQuant $ ForSome "a1" "a" ( QFind $ findParents $ Right "b" :: QIGI ) )

test_noIntroducedVarMasked = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> Right S.empty) S.empty
      [x,x1,x2,y,y1,y2] = ["x","x1","x2","y","y1","y2"]
      qx  = QQuant $ ForSome  "x" x qf
      qy  = QQuant $ ForSome  "y" y qf
      qx1 = QQuant $ ForSome "x1" x qf
      qy1 = QQuant $ ForSome "y1" y qf
      qxy = QJunct $ QOr [qx1,qy1]
      t = noIntroducedVarMasked

  assertBool "-1"  $ isLeft  $ t (QQuant $ ForSome x y qx)
  assertBool "0"   $ isRight $ t (QQuant $ ForSome x x qy)
  assertBool "1"   $ isRight $ t (QQuant $ ForSome x2 y qx1)
  assertBool "2"   $ isRight $ t (QQuant $ ForSome y x qx1)
  assertBool "3"   $ isRight $ t qx
  assertBool "3.7" $ isRight $ t qxy
  assertBool "4"   $ isRight $ t (QJunct $ QAnd [qx1,qxy])


test_noAndCollisions = TestCase $ do
  let qf  = QFind $ Find (\_ _ -> Right S.empty) S.empty
      [x,x1,x2,y,y1,y2,z] = ["x","x1","x2","y","y1","y2","z"]
      qx  = QQuant $ ForSome  x x qf
      qy  = QQuant $ ForSome  y y qf
      qx1 = QQuant $ ForSome x1 x qf
      qy1 = QQuant $ ForSome y1 y qf
      qxy = QJunct $ QOr [qx1,qy1]

  assertBool "-1"  $ isRight $ noAndCollisions (QQuant $ ForSome x y qx)
  assertBool "0"   $ isRight $ noAndCollisions (QQuant $ ForSome x x qy)
  assertBool "3"   $ isRight $ noAndCollisions qx
  assertBool "3.5" $ isRight $ noAndCollisions qx1
  assertBool "3.7" $ isRight $ noAndCollisions qxy
  assertBool "4"   $ isLeft  $ noAndCollisions ( QQuant $ ForSome z z
                                                 $ QJunct $ QAnd [qx1,qxy] )
  assertBool "5"   $ isLeft  $ noAndCollisions (QJunct $ QAnd [qx1,qxy])
  assertBool "6"   $ isRight $ noAndCollisions (QJunct $ QOr [qx1,qxy])

test_drawsFromVars = TestCase $ do
  let [a,b,c,d,e,f,g,h,x,y,z] = ["a","b","c","d","e","f","g","h","x","y","z"]
      -- These queries are named for their internal and external variables.
      c_de, c_a :: QIGI
      c_de = QQuant $ ForSome c d -- d was: (Source' d $ S.singleton a)
        $ QFind $ findParents $ Right e
      c_a = QQuant $ ForSome c a -- a was: (Source' a $ S.singleton g)
        $ QFind $ findParents $ Right c

  assertBool "5" $ drawsFromVars (QJunct $ QOr [ c_de, c_a ] :: QIGI)
                                      == S.fromList [d,a]
  assertBool "4" $ drawsFromVars c_de == S.singleton d
  assertBool "3" $ drawsFromVars c_a  == S.singleton a
  assertBool "2" $ drawsFromVars (QFind $ findParents $ Right c :: QIGI)
                                      == S.empty
  assertBool "1" $ drawsFromVars
    ( QQuant $ ForAll a b [] ( QJunct $ QOr [ c_de, c_a ] ) )
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
    (QJunct $ QOr [ c_de, c_a ] :: QIGI)
    == S.fromList [c,e]
  assertBool "4" $ usesVars c_de
    == S.singleton e
  assertBool "3" $ usesVars c_a
    == S.singleton c
  assertBool "1" $ usesVars
    ( QQuant $ ( ForAll a b
                 [ QVTest $ mkVTestCompare (>) (Left 1) (Right f) ]
                 ( QJunct $ QOr [ c_de, c_a ] )
               ) )
    == S.fromList [c,e,f]

test_introducesVars = TestCase $ do
  let [a,b,c,x,y,z] = ["a","b","c","x","y","z"]
      q = QFind $ Find (\_ _ -> Right $ S.singleton 1) S.empty
      meh = error "whatever"
  assertBool "1" $ introducesVars
    ( QJunct $ QOr [ QQuant $ ForAll x x [] q
                   , QQuant $ ForAll y y []
                     (QJunct $ QAnd [ QQuant $ ForSome z z q ] )
                   ] )
    == S.fromList [x,y,z]

test_findlike = TestCase $ do
  let qf = QFind $ Find (\_ _    -> Right S.empty) S.empty
      qc = QTest $ Test (\_ _  _ -> Right False  ) S.empty
  assertBool "1" $ findlike (QJunct $ QAnd [qf, qc]) == True
  assertBool "2" $ findlike (QJunct $ QAnd [qf]    ) == True
  assertBool "3" $ findlike (QJunct $ QAnd [qc]    ) == False
  assertBool "4" $ findlike (QJunct $ QAnd []      ) == False
  assertBool "5" $ findlike (QJunct $ QOr  [qf, qc]) == False
  assertBool "6" $ findlike (QJunct $ QOr  [qf]    ) == True
  assertBool "7" $ findlike (QJunct $ QOr  [qc]    ) == False
  assertBool "8" $ findlike (QJunct $ QOr  []      ) == False
