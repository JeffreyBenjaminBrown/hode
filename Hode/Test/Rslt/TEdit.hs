{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TEdit where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Lens.Micro hiding (has)
import           Test.HUnit

import           Hode.Rslt.RLookup hiding (exprToAddr)
import qualified Hode.Rslt.Edit as R
import qualified Hode.Rslt.Edit.Initial as R
import           Hode.Rslt.Index
import           Hode.Rslt.RTypes
import           Hode.Rslt.RValid
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_edit :: Test
test_module_rslt_edit = TestList [
    TestLabel "test_insert" test_insert
  , TestLabel "test_deleteIfUnused" test_deleteIfUnused
  , TestLabel "test_replaceInRole" test_replaceInRole
  , TestLabel "test_replace" test_replace
  , TestLabel "test_exprToAddrInsert" test_exprToAddrInsert
  , TestLabel "test_replaceExpr" test_replaceExpr
  , TestLabel "test_renameAddr_unsafe" test_renameAddr_unsafe
  ]

test_renameAddr_unsafe :: Test
test_renameAddr_unsafe = TestCase $ do
  let x = M.fromList [(0,Phrase' "")
                     ,(1,Tplt' [0,0,0])
                     ,(2,Phrase' "x")
                     ,(3,Phrase' "y")
                     ,(4,Rel' (Rel [2,3] 1))
                     ,(5,Rel' (Rel [4,2] 1))]
      r :: Rslt = mkRslt x
      s :: Rslt = mkRslt $ M.delete 4
                  $ M.insert 6 (Rel' (Rel [2,3] 1))
                  $ M.insert 5 (Rel' (Rel [6,2] 1)) x
  assertBool "1" $ R.renameAddr_unsafe 4 6 r == s

test_replaceExpr :: Test
test_replaceExpr = TestCase $ do
  let newRel      :: Rel Addr = Rel [1,3] 4
      new_refExpr :: RefExpr  = Rel' newRel
      new_expr    :: Expr     = ExprRel $ fmap Addr newRel
      refExprs    :: M.Map Addr RefExpr =
        D.refExprs & M.insert 5 new_refExpr
  assertBool "1" $ R.replaceExpr 5 new_expr D.rslt ==
    Right (mkRslt refExprs)

test_exprToAddrInsert :: Test
test_exprToAddrInsert = TestCase $ do
  assertBool "1" $ R.exprToAddrInsert D.rslt ( ExprTplt [ Addr 0
                                                     , Addr 3
                                                     , Addr 0 ] )
    == Right (D.rslt, 4)
  assertBool "2" $ R.exprToAddrInsert D.rslt ( ExprTplt [ Addr 0
                                                     , Addr 1
                                                     , Addr 0 ] )
    == Right ( fromRight (error "wut") $ R.insertAt 7 (Tplt' [0,1,0]) D.rslt
             , 7 )

  assertBool "3" $ R.exprToAddrInsert D.rslt ( ExprTplt [ Phrase "bar"
                                                     , Phrase ""
                                                     , Phrase "foo" ] )
    == Right ( fromRight (error "wut")
               $ R.insertAt 9 (Tplt' [7,0,8])
               $ fromRight (error "wut")
               $ R.insertAt 8 (Phrase' "foo")
               $ fromRight (error "wut")
               $ R.insertAt 7 (Phrase' "bar") D.rslt
             , 9 )

  assertBool "5" $ let
    Right (r,a) = R.exprToAddrInsert D.rslt
                  ( ExprRel $ Rel [ ExprRel $ Rel [ Phrase "space"
                                                  , Phrase "empty" ]
                               ( ExprTplt [ Phrase ""
                                           , Phrase "is"
                                           , Addr 0 ] )
                             , Phrase "suck" ]
                    ( ExprTplt [ Phrase "That"
                                , Phrase "does"
                                , Addr 0 ] ) )
    (n16 :: Expr) =
      either (error "wut") id $ addrToRefExpr r a >>= refExprToExpr r
    in eShow r n16 == Right "##That space #is empty ##does suck"

test_replace :: Test
test_replace = TestCase $ do
  assertBool "replace word in rel" $
    either (error "wut") id (R.replaceRefExpr (Phrase' "foo") 1 D.rslt)
    == mkRslt ( M.fromList
          [ (0, Phrase' "")
          , (2, Phrase' "oxygen")
          , (3, Phrase' "needs")
          , (4, Tplt' [0,3,0])
          , (5, Rel' $ Rel [7,2] 4) -- all changes involve address 7
          , (6, Rel' $ Rel [5,2] 4)
          , (7, Phrase' "foo")
          ] )

  assertBool "replace word in Tplt" $
    either (error "wut") id (R.replaceRefExpr (Phrase' "foo") 0 D.rslt)
    == mkRslt ( M.fromList
         [ (7, Phrase' "foo")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (4, Tplt' [7,3,7]) -- all changes involve address 7
         , (5, Rel' $ Rel [1,2] 4)
         , (6, Rel' $ Rel [5,2] 4)
         ] )

  assertBool "replace rel" $
    either (error "wut") id (R.replaceRefExpr (Rel' $ Rel [2,1] 4) 5 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (4, Tplt' [0,3,0])
         , (6, Rel' $ Rel [7,2] 4)
         , (7, Rel' $ Rel [2,1] 4) -- all changes involve address 7
         ] )

  assertBool "todo : replace tplt" $
    either (error "wut") id (R.replaceRefExpr (Tplt' [2,2,2]) 4 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         -- all changes involve address 7
         , (7, Tplt' [2,2,2])
         , (5, Rel' $ Rel [1,2] 7)
         , (6, Rel' $ Rel [5,2] 7)
         ] )

test_replaceInRole :: Test
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
  assertBool "2" $ isIn r 6 == Right S.empty
  assertBool "3" $ has r 5 == Right ( M.fromList [ (RoleMember 1, 1)
                                                 , (RoleMember 2, 1)
                                                 , (RoleTplt    , 4) ] )

  let r2 = either (error "wut") id
           $ R.replaceInRole (RoleMember 2) 8 5
           $ either (error "wut") id
           $ R.insertAt 8 (Phrase' "foo") D.rslt
  assertBool "4" $ isIn r2 8 == Right (S.singleton (RoleMember 2, 5))

test_deleteIfUnused :: Test
test_deleteIfUnused = TestCase $ do
  -- TODO : now that Expr 6 is deleted, this test does not do what it claims.
  -- from D.rslt, remove the Par called 6 (because it uses the Rel'5)
  -- and insert at 6 (Rel' $ Rel [1,1] 4), before deleting at 5 (Rel'(1,2) 4).
  -- Now 1 should be in the new rel and not the old, and 2 should be in nothing.
  let (without_6    :: Rslt) = mkRslt $ M.delete 6 D.refExprs
      (with_new_rel :: Rslt) = either (error "wut") id
                               $ R.insertAt 6 (Rel' $ Rel [1,1] 4) without_6
      (r            :: Rslt) = either (error "wut") id
                               $ R.deleteIfUnused 5 with_new_rel
  assertBool "valid 1" $ isRight $ validRslt without_6
  assertBool "valid 2" $ isRight $ validRslt with_new_rel
  assertBool "valid 3" $ isRight $ validRslt r

  assertBool "1" $ isLeft $ R.deleteIfUnused 5 D.rslt
  assertBool "addrToRefExpr of deleted" $ isLeft $ addrToRefExpr r 5
  assertBool "refExprToAddr missing"    $ isLeft $
    either (error "wut") (refExprToAddr r) (addrToRefExpr D.rslt 5)
  assertBool "variety missing"   $ isLeft $ variety r 5
  assertBool "has missing"       $ isLeft $ has r 5
  assertBool "isIn missing"      $ isLeft $ isIn r 5
  assertBool "isIn $ former member of missing" $
    isIn r 1 == Right ( S.fromList [ (RoleMember 1, 6)
                                   , (RoleMember 2, 6) ] )
  assertBool "isIn $ another former member of missing" $
    isIn r 2 == Right S.empty

test_insert :: Test
test_insert = TestCase $ do
  let r2 = either (error "wut") id
           $ R.insertAt 7 (Rel' $ Rel [1,1] 4) D.rslt
  assertBool "valid 1" $ isRight $ validRslt r2

  assertBool "1" $ isIn r2 4 == Right (S.fromList [ (RoleTplt    , 7     )
                                                  , (RoleTplt    , 6     )
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
    R.insertAt 1 (Phrase' "nuyck") D.rslt
  assertBool "non-matching Tplt" $ isLeft $
    R.insertAt 1 (Rel' $ Rel [1,2,3] 4) D.rslt
  assertBool "nonexistent references" $ isLeft $
    R.insertAt 1 (Rel' $ Rel [11,22] 4) D.rslt
