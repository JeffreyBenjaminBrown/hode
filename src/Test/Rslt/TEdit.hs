{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.TEdit where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Data.Rslt.Lookup hiding (insert, lookup)
import qualified Data.Rslt.Edit as R
import           Data.Rslt.Index
import qualified Data.Rslt.Lookup as R
import           Data.Rslt.RTypes
import           Data.Rslt.RValid
import           Data.Rslt.Show
import qualified Test.Rslt.RData as D
import           Util


test_module_rslt_edit = TestList [
    TestLabel "test_insert" test_insert
  , TestLabel "test_deleteUnusedExpr" test_deleteUnusedExpr
  , TestLabel "test_replaceInRole" test_replaceInRole
  , TestLabel "test_replace" test_replace
  ]

test_replace = TestCase $ do
  assertBool "replace word in rel" $
    either (error "wut") id (R.replace (Word "foo") 1 D.rslt)
    == mkRslt ( M.fromList
          [ (0, Word "")
          , (2, Word "oxygen")
          , (3, Word "needs")
          , (4, Tplt [0,3,0])
          , (5, Rel [7,2] 4) -- all changes involve address 7
          , (6, Par [("The first relationship in this graph is ", 5)] ".")
          , (7, Word "foo")
          ] )

  assertBool "replace word in template" $
    either (error "wut") id (R.replace (Word "foo") 0 D.rslt)
    == mkRslt ( M.fromList
         [ (7, Word "foo")
         , (1, Word "dog")
         , (2, Word "oxygen")
         , (3, Word "needs")
         , (4, Tplt [7,3,7]) -- all changes involve address 7
         , (5, Rel [1,2] 4)
         , (6, Par [("The first relationship in this graph is ", 5)] ".")
         ] )

  assertBool "replace rel" $
    either (error "wut") id (R.replace (Rel [2,1] 4) 5 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Word "")
         , (1, Word "dog")
         , (2, Word "oxygen")
         , (3, Word "needs")
         , (4, Tplt [0,3,0])
         , (7, Rel [2,1] 4) -- all changes involve address 7
         , (6, Par [("The first relationship in this graph is ", 7)] ".")
         ] )

  assertBool "todo : replace tplt" $
    either (error "wut") id (R.replace (Tplt [2,2,2]) 4 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Word "")
         , (1, Word "dog")
         , (2, Word "oxygen")
         , (3, Word "needs")
         , (7, Tplt [2,2,2])
         , (5, Rel [1,2] 7) -- all changes involve address 7
         , (6, Par [("The first relationship in this graph is ", 5)] ".")
         ] )

test_replaceInRole = TestCase $ do
  let r         = either (error "wut") id $
                  R.replaceInRole (RoleMember 2) 1 5 D.rslt
      unchanged = either (error "wut") id $
                  R.replaceInRole (RoleMember 2) 2 5 D.rslt
  assertBool "valid 1" $ isRight $ validRslt r
  assertBool "valid 2" $ isRight $ validRslt unchanged
  assertBool "identity" $ D.rslt == unchanged
  assertBool "1" $ isIn r 1 == Right ( S.fromList [ (RoleMember 1, 5)
                                                  , (RoleMember 2, 5) ] )
  assertBool "2" $ isIn r 2 == Right S.empty
  assertBool "3" $ has r 5 == Right ( M.fromList [ (RoleMember 1, 1)
                                                 , (RoleMember 2, 1)
                                                 , (RoleTplt    , 4) ] )

  let r2 = either (error "wut") id
           $ R.replaceInRole (RoleMember 2) 8 5
           $ either (error "wut") id
           $ R.insertAt 8 (Word "foo") D.rslt
  assertBool "4" $ isIn r2 8 == Right (S.singleton (RoleMember 2, 5))

test_deleteUnusedExpr = TestCase $ do
  -- from D.rslt, remove the Par called 6 (because it uses the Rel 5)
  -- and insert at 6 (Rel [1,1] 4), before deleting at 5 (Rel (1,2) 4).
  -- Now 1 should be in the new rel and not the old, and 2 should be in nothing.
  let (without_6    :: Rslt) = mkRslt $ M.delete 6 D.exprs
      (with_new_rel :: Rslt) = either (error "wut") id
                               $ R.insertAt 6 (Rel [1,1] 4) without_6
      (r            :: Rslt) = either (error "wut") id
                               $ R.deleteUnusedExpr 5 with_new_rel
  assertBool "valid 1" $ isRight $ validRslt without_6
  assertBool "valid 2" $ isRight $ validRslt with_new_rel
  assertBool "valid 3" $ isRight $ validRslt r

  assertBool "1" $ isLeft $ R.deleteUnusedExpr 5 D.rslt
  assertBool "exprAt of deleted" $ isLeft $ exprAt r 5
  assertBool "addrOf missing"    $ isLeft $
    either (error "wut") (addrOf r) (exprAt D.rslt 5)
  assertBool "variety missing"   $ isLeft $ variety r 5
  assertBool "has missing"       $ isLeft $ has r 5
  assertBool "isIn missing"      $ isLeft $ isIn r 5
  assertBool "isIn $ former member of missing" $
    isIn r 1 == Right ( S.fromList [ (RoleMember 1, 6)
                                   , (RoleMember 2, 6) ] )
  assertBool "isIn $ another former member of missing" $
    isIn r 2 == Right S.empty

test_insert = TestCase $ do
  let r2 = either (error "wut") id
           $ R.insertAt 7 (Rel [1,1] 4) D.rslt
  assertBool "valid 1" $ isRight $ validRslt r2

  assertBool "1" $ isIn r2 4 == Right (S.fromList [ (RoleTplt    , 7     )
                                                  , (RoleTplt    , 5     ) ] )
  assertBool "2" $ isIn r2 1 == Right (S.fromList [ (RoleMember 1, 7     )
                                                  , (RoleMember 2, 7     )
                                                  , (RoleMember 1, 5     ) ] )
  assertBool "3" $ has r2 7 == Right ( M.fromList [ (RoleMember 1, 1     )
                                                  , (RoleMember 2, 1     )
                                                  , (RoleTplt    , 4     ) ] )
  assertBool "4" $ map (has D.rslt) [1..6] == map (has r2) [1..6]
  assertBool "5" $ isLeft $ has D.rslt  7

  assertBool "address collision" $ isLeft $
    R.insertAt 1 (Word "nuyck") D.rslt
  assertBool "non-matching template" $ isLeft $
    R.insertAt 1 (Rel [1,2,3] 4) D.rslt
  assertBool "nonexistent references" $ isLeft $
    R.insertAt 1 (Rel [11,22] 4) D.rslt
