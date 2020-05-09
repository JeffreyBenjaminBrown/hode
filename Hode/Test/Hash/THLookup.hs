{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.THLookup where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import Hode.Hash.Convert
import Hode.Hash.Lookup
import Hode.Hash.Types
import Hode.Qseq.Types (Var(..))
import Hode.Rslt.Edit.Initial (insertAt)
import Hode.Rslt.Index
import Hode.Rslt.Lookup
import Hode.Rslt.Types
import Hode.NoUI
import qualified Hode.Test.Rslt.RData as D


vs :: String -> Var
vs = VarString

test_module_hash_lookup :: Test
test_module_hash_lookup = TestList [
    TestLabel "test_subExprs" test_subExprs
  , TestLabel "test_hExprToAddrs" test_hExprToAddrs
  , TestLabel "test_hExprToExpr" test_hExprToExpr
  , TestLabel "testHMatches" testHMatches
  , TestLabel "testFirstAbsent" testFirstAbsent
  , TestLabel "test_usesTransitiveTplt" test_usesTransitiveTplt
  ]

test_usesTransitiveTplt :: Test
test_usesTransitiveTplt = TestCase $ do
  let Right (r :: Rslt) = nInserts (mkRslt mempty)
        [ "0 #a 1"
        , "0 #b 1"
        , "(/t /_ a /_) #is transitive" ]
  assertBool "(0 #a 1) uses a transitive tplt" $ let
    x = do a <- fst . head <$> nFind r "0 #a 1"
           usesTransitiveTplt r a
    in x == Right True
  assertBool "(0 #b 1) doesn't" $ let
    x = do a <- fst . head <$> nFind r "0 #b 1"
           usesTransitiveTplt r a
    in x == Right False

testFirstAbsent :: Test
testFirstAbsent = TestCase $ do
  let s :: String = "# /_ # a"
      p :: PExpr = PRel $ Open 1
        [Absent,PNonRel Any,PNonRel $ PExpr $ Phrase "a"]
        ["",""]
      hGood = HMap $ M.fromList
        [ ( RoleInRel' $ RoleTplt, HExpr $ ExprTplt $ Tplt
                      (Just $ Phrase "") [Phrase ""] Nothing),
          ( RoleInRel' $ RoleMember 2, HExpr $ Phrase "a") ]

  assertBool "parse" $
    nPExpr s == Right p
  assertBool "convert" $
    pExprToHExpr (mkRslt mempty) p == Right hGood

  let Right r  = fst' <$> nInsert (mkRslt mempty) "# a # b"
        where fst' (x,_,_) = x
      Right t  = nFind r "# /_ # /_"
      Right t2 = nFind r "# /_ # b"
  assertBool "1" $ length t  == 1
  assertBool "2" $ length t2 == 1

testHMatches :: Test
testHMatches = TestCase $ do
  -- hMatches :: Rslt -> HExpr -> Addr -> Either String Bool
  let r = D.b2
  assertBool "HExpr" $ hMatches r (HExpr $ Phrase "fish") 2 == Right True
  assertBool "HExpr false" $ hMatches r (HExpr $ Phrase "donkey") 2
    == Right False
  assertBool "HExpr left" $ isLeft $
    hMatches r (HExpr $ Phrase "donkey") 1000

  assertBool "HMap" $ hMatches r
    ( HMap $ M.singleton (RoleInRel' $ RoleMember 1)
      (HExpr $ Phrase "fish") ) 10
    == Right True
  assertBool "HMap false" $ hMatches r
    ( HMap $ M.singleton (RoleInRel' $ RoleMember 1)
      (HExpr $ Phrase "merp") ) 10
    == Right False

  assertBool "HEval" $ hMatches r
    (HEval (HExpr $ ExprAddr 10) [[RoleMember 1]]) 2
    == Right True
  assertBool "HEval false" $ hMatches r
    (HEval (HExpr $ ExprAddr 10) [[RoleMember 2]]) 2
    == Right False

test_hExprToExpr :: Test
test_hExprToExpr = TestCase $ do
  assertBool "no Tplt" $ isLeft $ hExprToExpr D.big $ HMap M.empty
  assertBool "false Tplt" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.singleton (RoleInRel' $ RoleTplt) $ HExpr $ Phrase "galk"
  assertBool "non-convertible member" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.fromList [ (RoleInRel' $ RoleTplt    , HExpr $ Phrase "galk")
                 , (RoleInRel' $ RoleMember 1, HVar (vs "x")) ]
  assertBool "arity mismatch" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.singleton (RoleInRel' $ RoleTplt) $ HExpr $ ExprAddr 4
  assertBool "good" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.fromList [ (RoleInRel' $ RoleTplt    , HExpr $ ExprAddr 4)
                 , (RoleInRel' $ RoleMember 1, HExpr $ Phrase "yo!") ]

test_hExprToAddrs :: Test
test_hExprToAddrs = TestCase $ do
  -- TODO ? Still untested:
    -- One HEval inside another

  assertBool "find 2" $ hExprToAddrs D.big M.empty
    ( HExpr $ ExprAddr 2)
    == Right ( S.fromList [2] )

  assertBool "9 is the only Expr with 2 as member 1"
    $ hExprToAddrs D.big M.empty
    ( HMap $ M.fromList [ ( RoleInRel' $ RoleMember 1,
                            HExpr $ ExprAddr 2) ] )
    == Right ( S.fromList [9] )

  assertBool "nothing has 10 as its first member."
    $ hExprToAddrs D.big M.empty
    ( HMap $ M.fromList [ ( RoleInRel' $ RoleMember 1,
                            HExpr $ ExprAddr 10) ] )
    == Right S.empty

  assertBool "2 is the first member of the only thing (9) that has 2 as its first member."
    $ hExprToAddrs D.big M.empty
    ( HEval
      ( HMap $ M.fromList [ (RoleInRel' $ RoleMember 1,
                             HExpr $ ExprAddr 2) ] )
      [ [ RoleMember 1 ] ] )
    == Right ( S.fromList [2] )

  assertBool "9 is the only Expr in D.big whose 2nd member is 3."
    $ hExprToAddrs D.big M.empty
    ( HMap ( M.singleton (RoleInRel' $ RoleMember 2)
             $ HExpr $ Phrase "3" ) )
    == Right ( S.fromList [9] )

  assertBool "2 is the first member of the only thing (9) that has 3 as its second member."
    $ hExprToAddrs D.big M.empty
    ( HEval
      ( HMap $ M.fromList [ (RoleInRel' $ RoleMember 2,
                             HExpr $ ExprAddr 3) ] )
      [ [ RoleMember 1 ] ] )
    == Right ( S.fromList [2] )

  assertBool "9 is the only thing whose first member is 2. The HEval returns 2. 7 is the only thing whose second member is equal to what that HEval returned, so 7 is what the outer HMap returns."
    $ hExprToAddrs D.big M.empty
    ( HMap ( M.singleton (RoleInRel' $ RoleMember 2)
             $ HEval
             ( HMap $ M.fromList [ (RoleInRel' $ RoleMember 1,
                                    HExpr $ ExprAddr 2) ] )
             [ [ RoleMember 1 ] ] ) )
    == Right ( S.fromList [7] )

  let Right r = nInserts (mkRslt mempty)
                [ "a ## b # c"
                , "e # f ## g" ]
      addrOf :: String -> Addr
      addrOf s = either (error "absent") id
                 $ fst . head <$> nFind r s

    in do
    assertBool "Find something with \"a\" as its first member. Return the first member of its second member." $
      ( hExprToAddrs r M.empty $ HEval
        ( HMap $ M.fromList [ ( RoleInRel' $ RoleMember 1,
                                HExpr $ Phrase "a") ] )
        [ [ RoleMember 2, RoleMember 1 ] ] )
      == Right (S.singleton $ addrOf "b")

    assertBool "Find something with \"g\" as its first member. Return the first member of its first member." $
      ( hExprToAddrs r M.empty $ HEval
        ( HMap $ M.fromList [ (RoleInRel' $ RoleMember 2,
                               HExpr $ Phrase "g") ] )
        [ [ RoleMember 1, RoleMember 1 ] ] )
      == Right (S.singleton $ addrOf "e")


test_subExprs :: Test
test_subExprs = TestCase $ do
  let r = fromRight (error "wut") $ insertAt 7 (Rel' $ Rel [5,5] 4) D.rslt
  assertBool "1" $ subExprs r [ [RoleMember 1, RoleMember 1] ] 7
    == Right (S.fromList [1])
  assertBool "2" $ subExprs r [ [RoleMember 2, RoleMember 2] ] 7
    == Right (S.fromList [2])
  assertBool "3" $ subExprs r [ [RoleMember 2, RoleMember 1] ] 7
    == Right (S.fromList [1])
  assertBool "3" $ subExprs r [ [RoleMember 2, RoleMember 1]
                              , [RoleMember 1]
                              ] 7
    == Right (S.fromList [1,5])
  assertBool "4" $ subExprs r [ [RoleMember 2, RoleMember 1]
                              , [RoleMember 1, RoleMember 1]
                              ] 7
    == Right (S.fromList [1])
  assertBool "5" $ subExprs r [ [RoleMember 1, RoleMember 1]
                              , [RoleMember 1, RoleMember 2]
                              , [RoleMember 2, RoleMember 2]
                              ] 7
    == Right (S.fromList [1,2])
