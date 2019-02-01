{-# LANGUAGE ScopedTypeVariables #-}

module Test.TRslt where

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
import qualified Test.Rslt.RData as D
import           Util


test_module_rslt = TestList [
  TestLabel "test_variety" test_variety
  , TestLabel "test_fills" test_fills
  , TestLabel "test_isIn" test_isIn
  , TestLabel "test_has" test_has
  , TestLabel "test_lookup" test_lookup
  , TestLabel "test_insert" test_insert
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

repRel = either (error "wut") id (R.replace (Tplt [2,2,2]) 4 D.rslt)
repList = mkRslt ( M.fromList
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
  assertBool "addrOf missing"    $ Nothing ==
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

test_lookup = TestCase $ do
  assertBool "1" $ (R.lookup D.rslt $ ImgOfAddr 0)              == Just 0
  assertBool "2" $ (R.lookup D.rslt $ ImgOfAddr $ -10000)       == Nothing
  assertBool "3" $ (R.lookup D.rslt $ ImgOfExpr $ Word "needs") == Just 3
  assertBool "4" $ (R.lookup D.rslt $ ImgOfExpr $ Tplt [0,3,0]) == Just 4
  assertBool "5" $ Just 4 ==
    R.lookup D.rslt ( ImgOfTplt [ ImgOfAddr 0
                                , ImgOfExpr $ Word "needs"
                                , ImgOfExpr $ Word ""] )

  assertBool "6" $ Just 5 ==
    R.lookup D.rslt ( ImgOfRel [ ImgOfAddr 1
                               , ImgOfExpr $ Word "oxygen"]
                      $ ImgOfAddr 4 )
  assertBool "7" $ Nothing ==
    R.lookup D.rslt ( ImgOfRel [ ImgOfAddr 1
                               , ImgOfExpr $ Word "oxygen"]
                      $ ImgOfAddr 6 )

test_has = TestCase $ do
  assertBool "tplt" $ has D.rslt 4
    == Right ( M.fromList [ ( RoleMember 1, 0 )
                         , ( RoleMember 2, 3 )
                         , ( RoleMember 3, 0 ) ] )
  assertBool "rel" $ has D.rslt 5
    == Right ( M.fromList [ ( RoleMember 1, 1 )
                         , ( RoleMember 2, 2 )
                         , ( RoleTplt    , 4 ) ] )
  assertBool "par" $ has D.rslt 6
    == Right ( M.fromList [ ( RoleMember 1, 5 ) ] )
  assertBool "no content" $ has D.rslt 0 == Right M.empty
  assertBool "absent" $ isLeft $ has D.rslt 7

test_isIn = TestCase $ do
  assertBool "1" $ M.lookup 0 (_isIn D.rslt)
    == Just ( S.fromList [ (RoleMember 1, 4)
                         , (RoleMember 3, 4) ] )
  assertBool "2" $ M.lookup 4 (_isIn D.rslt)
    == Just ( S.fromList [ (RoleTplt, 5) ] )
  assertBool "3" $ let r' = either (error "wut") id
                            $ R.insertAt 7 (Word "pizza") D.rslt
                   in isIn r' 7 == Right S.empty

test_fills = TestCase $ do
  assertBool "1st in tplt"
    $ fills D.rslt (RoleMember 1, 4) == Right 0
  assertBool "2nd in tplt"
    $ fills D.rslt (RoleMember 2, 4) == Right 3
  assertBool "1st in rel"
    $ fills D.rslt (RoleMember 2, 5) == Right 2
  assertBool "2nd in rel"
    $ fills D.rslt (RoleMember 1, 5) == Right 1
  assertBool "nonexistent (3rd in binary)" $ isLeft
    $ fills D.rslt (RoleMember 3, 5)
  assertBool "tplt in rel"
    $ fills D.rslt (RoleTplt    , 5) == Right 4
  assertBool "nonexistent (tplt in par)" $ isLeft
    $ fills D.rslt (RoleTplt    , 6)
  assertBool "first in par"
    $ fills D.rslt (RoleMember 1, 6) == Right 5

test_variety = TestCase $ do
  assertBool "1" $ M.lookup 3      (_variety D.rslt) == Just (Word',0)
  assertBool "2" $ M.lookup 4      (_variety D.rslt) == Just (Tplt',2)
  assertBool "3" $ M.lookup 5      (_variety D.rslt) == Just (Rel',2)
  assertBool "4" $ M.lookup 6      (_variety D.rslt) == Just (Par',1)
  assertBool "5" $ M.lookup (-133) (_variety D.rslt) == Nothing
