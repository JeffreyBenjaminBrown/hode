{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hash.THLookup where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import Hash.HLookup
import Hash.HTypes
import Rslt.Edit
import Rslt.RTypes
import Rslt.RLookup
import qualified Test.Rslt.RData as D


test_module_rslt_hash :: Test
test_module_rslt_hash = TestList [
    TestLabel "test_subExprs" test_subExprs
  , TestLabel "test_hExprToAddrs" test_hExprToAddrs
  , TestLabel "test_hExprToExpr" test_hExprToExpr
  ]

test_hExprToExpr :: Test
test_hExprToExpr = TestCase $ do
  assertBool "no Tplt" $ isLeft $ hExprToExpr D.big $ HMap M.empty
  assertBool "false Tplt" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.singleton (RoleTplt) $ HExpr $ Phrase "galk"
  assertBool "non-convertible member" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.fromList [ (RoleTplt    , HExpr $ Phrase "galk")
                 , (RoleMember 1, HVar "x") ]
  assertBool "arity mismatch" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.singleton RoleTplt $ HExpr $ Addr 4
  assertBool "good" $ isLeft $ hExprToExpr D.big $ HMap
    $ M.fromList [ (RoleTplt    , HExpr $ Addr 4)
                 , (RoleMember 1, HExpr $ Phrase "yo!") ]

test_hExprToAddrs :: Test
test_hExprToAddrs = TestCase $ do
  -- TODO ? Still untested:
    -- HEval paths of length > 1
    -- One HEval inside another

  assertBool "find 2" $ hExprToAddrs D.big M.empty
    ( HExpr $ Addr 2)
    == Right ( S.fromList [2] )

  assertBool "9 is the only Expr with 2 as member 1"
    $ hExprToAddrs D.big M.empty
    ( HMap $ M.fromList [ ( RoleMember 1, HExpr $ Addr 2) ] )
    == Right ( S.fromList [9] )

  assertBool "nothing has 10 as its first member."
    $ hExprToAddrs D.big M.empty
    ( HMap $ M.fromList [ ( RoleMember 1, HExpr $ Addr 10) ] )
    == Right S.empty

  assertBool "2 is the first member of the only thing (9) that has 2 as its first member. (Duh.)"
    $ hExprToAddrs D.big M.empty
    ( HEval
      ( HMap $ M.fromList [ (RoleMember 1, HExpr $ Addr 2) ] )
      [ [ RoleMember 1 ] ] )
    == Right ( S.fromList [2] )

  assertBool "9 is the only Expr in D.big whose 2nd member is 3."
    $ hExprToAddrs D.big M.empty
    ( HMap ( M.singleton (RoleMember 2)
             $ HExpr $ Phrase "3" ) )
    == Right ( S.fromList [9] )

  assertBool "2 is the first member of the only thing (9) that has 3 as its second member."
    $ hExprToAddrs D.big M.empty
    ( HEval
      ( HMap $ M.fromList [ (RoleMember 2, HExpr $ Addr 3) ] )
      [ [ RoleMember 1 ] ] )
    == Right ( S.fromList [2] )

  assertBool "9 is the only thing whose first member is 2. The HEval returns 2. 7 is the only thing whose second member is equal to what that HEval returned, so 7 is what the outer HMap returns."
    $ hExprToAddrs D.big M.empty
    ( HMap ( M.singleton (RoleMember 2)
             $ HEval
             ( HMap $ M.fromList [ (RoleMember 1, HExpr $ Addr 2) ] )
             [ [ RoleMember 1 ] ] ) )
    == Right ( S.fromList [7] )

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
