{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TEdit where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Lens.Micro hiding (has)
import           Test.HUnit

import           Hode.Rslt.RLookup hiding (exprToAddr)
import qualified Hode.Rslt.Edit         as R
import qualified Hode.Rslt.Edit.Initial as R
import qualified Hode.Rslt.Edit.Replace as R
import           Hode.Rslt.Index
import           Hode.Rslt.RTypes
import           Hode.Rslt.RValid
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D
import           Hode.Util.Misc


test_module_rslt_edit :: Test
test_module_rslt_edit = TestList [
    TestLabel "test_insert" test_insert
  , TestLabel "test_deleteIfUnused" test_deleteIfUnused
  , TestLabel "test_replaceInRole" test_replaceInRole
  , TestLabel "test_replaceRefExpr" test_replaceRefExpr
  , TestLabel "test_exprToAddrInsert" test_exprToAddrInsert
  , TestLabel "test_replaceExpr" test_replaceExpr
  , TestLabel "test_renameAddr_unsafe" test_renameAddr_unsafe
  , TestLabel "test_replaceInRefExpr" test_replaceInRefExpr
  ]

test_replaceInRefExpr :: Test
test_replaceInRefExpr = TestCase $ do
  let r = mkRslt $ M.fromList
          [ (0, Phrase' "")
          , (1, Phrase' "dog")
          , (2, Phrase' "oxygen")
          , (3, Phrase' "needs")
          , (4, Tplt'$ Tplt Nothing  [3] $ Just 1)
          , (5, Tplt'$ Tplt (Just 1) [3] Nothing)
          , (6, Rel' $ Rel [1,2] 4)
          , (7, Rel' $ Rel [5,2] 4) ]
  assertBool "There's no LeftCap to replace" $
    isLeft $ R._replaceInRefExpr r
    (RoleInTplt' $ RoleCapLeft) 1
    (Tplt'$ Tplt Nothing [3] Nothing)
  assertBool "Replace RightCap 1 with 2" $
    R._replaceInRefExpr r
    (RoleInTplt' $ RoleCapRight) 2
             (Tplt' $ Tplt Nothing [3] $ Just 1)
    == Right (Tplt' $ Tplt Nothing [3] $ Just 2)
  assertBool "Replace first member with 2" $
    R._replaceInRefExpr r
    (RoleInRel' $ RoleMember 1) 2
             (Rel' $ Rel [1,2] 4)
    == Right (Rel' $ Rel [2,2] 4)
  assertBool "Replace Tplt with 6" $
    R._replaceInRefExpr r
    (RoleInRel' RoleTplt) 6
             (Rel' $ Rel [1,2] 5)
    == Right (Rel' $ Rel [1,2] 6)

test_renameAddr_unsafe :: Test
test_renameAddr_unsafe = TestCase $ do
  let x = M.fromList [(0,Phrase' "")
                     ,(1,Tplt' $ Tplt Nothing [0] Nothing)
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
      new_expr    :: Expr     = ExprRel $ fmap ExprAddr newRel
      refExprs    :: M.Map Addr RefExpr =
        D.refExprs & M.insert 5 new_refExpr
  assertBool "something" $ R.replaceExpr 5 new_expr D.rslt ==
    Right (mkRslt refExprs)

test_exprToAddrInsert :: Test
test_exprToAddrInsert = TestCase $ do
  assertBool "1" $ ( R.exprToAddrInsert D.rslt $ ExprTplt $ Tplt
                     Nothing [ExprAddr 3] Nothing )
    == Right (D.rslt, [Old 4])

  assertBool "2" $ ( R.exprToAddrInsert D.rslt $ ExprTplt $ Tplt
                     Nothing [ExprAddr 1] Nothing )
    == Right ( either error id $
               R.insertAt 7 (Tplt' $ Tplt Nothing [1] Nothing) D.rslt
             , [New 7, Old 1] )

  assertBool "3" $ ( R.exprToAddrInsert D.rslt $ ExprTplt $ Tplt
                     (Just $ Phrase "bar") [Phrase ""]
                     (Just $ Phrase "foo") )
    == Right ( either error id
               $ R.insertAt 9 (Tplt' $ Tplt (Just 7) [0] (Just 8))
               $ either error id
               $ R.insertAt 8 (Phrase' "foo")
               $ either error id
               $ R.insertAt 7 (Phrase' "bar") D.rslt
             , [New 9, New 7, Old 0, New 8] )

  assertBool "5" $ let
    Right (r,as) =
      R.exprToAddrInsert D.rslt
      ( ExprRel $ Rel [ ExprRel $ Rel [ Phrase "space"
                                      , Phrase "empty" ]
                        ( ExprTplt $ Tplt
                          Nothing [Phrase "is"] Nothing )
                      , Phrase "suck" ]
        ( ExprTplt $ Tplt
          (Just $ Phrase "That") [Phrase "does"] Nothing ) )
    a = unAged $ head as
    (n16 :: Expr) =
      either error id $ addrToRefExpr r a >>= refExprToExpr r
    in eShow r n16 == Right "##That space #is empty ##does suck"

test_replaceRefExpr :: Test
test_replaceRefExpr = TestCase $ do
  assertBool "replace word in rel" $ either error id
    (R.replaceRefExpr (Phrase' "foo") 1 D.rslt)
    == mkRslt ( M.fromList
          [ (0, Phrase' "")
          , (2, Phrase' "oxygen")
          , (3, Phrase' "needs")
          , (4, Tplt' $ Tplt Nothing [3] Nothing)
          , (5, Rel' $ Rel [7,2] 4) -- all changes involve address 7
          , (6, Rel' $ Rel [5,2] 4)
          , (7, Phrase' "foo")
          ] )

  assertBool "replace word in Tplt" $ either error id
    (R.replaceRefExpr (Phrase' "foo") 1 D.rslt_rightCapped)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (7, Phrase' "foo")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (4, Tplt' $ Tplt Nothing [3] $ Just 7)
           -- all changes involve changing addr 1 (dog) to 7 (foo)
         , (5, Rel' $ Rel [7,2] 4)
         , (6, Rel' $ Rel [5,2] 4)
         ] )

  assertBool "replace rel" $ either error id
    (R.replaceRefExpr (Rel' $ Rel [2,1] 4) 5 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (4, Tplt' $ Tplt Nothing [3] Nothing)
         , (6, Rel' $ Rel [7,2] 4)
         , (7, Rel' $ Rel [2,1] 4) -- all changes involve address 7
         ] )

  assertBool "todo : replace tplt" $ either error id
    (R.replaceRefExpr (Tplt' $ Tplt (Just 2) [2] (Just 2)) 4 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         -- all changes involve address 7
         , (7, Tplt' $ Tplt (Just 2) [2] (Just 2))
         , (5, Rel' $ Rel [1,2] 7)
         , (6, Rel' $ Rel [5,2] 7)
         ] )

test_replaceInRole :: Test
test_replaceInRole = TestCase $ do
  let r         = either error id $
                  R.replaceInRole (RoleInRel' $ RoleMember 2) 1 5 D.rslt
      unchanged = either error id $
                  R.replaceInRole (RoleInRel' $ RoleMember 2) 2 5 D.rslt
  assertBool "valid 1" $ isRight $ validRslt r
  assertBool "valid 2" $ isRight $ validRslt unchanged
  assertBool "identity" $ D.rslt == unchanged
  assertBool "1" $ isIn r 1 == Right
    ( S.fromList [ (RoleInRel' $ RoleMember 1, 5)
                 , (RoleInRel' $ RoleMember 2, 5) ] )
  assertBool "2" $ isIn r 6 == Right S.empty
  assertBool "3" $ has r 5 == Right
    ( M.fromList [ (RoleInRel' $ RoleMember 1, 1)
                 , (RoleInRel' $ RoleMember 2, 1)
                 , (RoleInRel' $ RoleTplt    , 4) ] )

  let r2 = either error id
           $ R.replaceInRole (RoleInRel' $ RoleMember 2) 8 5
           $ either error id
           $ R.insertAt 8 (Phrase' "foo") D.rslt
  assertBool "4" $ isIn r2 8 == Right
    (S.singleton (RoleInRel' $ RoleMember 2, 5))

test_deleteIfUnused :: Test
test_deleteIfUnused = TestCase $ do
  -- TODO : now that Expr 6 is deleted, this test does not do what it claims.
  -- from D.rslt, remove the Par called 6 (because it uses the Rel'5)
  -- and insert at 6 (Rel' $ Rel [1,1] 4), before deleting at 5 (Rel'(1,2) 4).
  -- Now 1 should be in the new rel and not the old, and 2 should be in nothing.
  let (without_6    :: Rslt) = mkRslt $ M.delete 6 D.refExprs
      (with_new_rel :: Rslt) = either error id
                               $ R.insertAt 6 (Rel' $ Rel [1,1] 4) without_6
      (r            :: Rslt) = either error id
                               $ R.deleteIfUnused 5 with_new_rel
  assertBool "valid 1" $ isRight $ validRslt without_6
  assertBool "valid 2" $ isRight $ validRslt with_new_rel
  assertBool "valid 3" $ isRight $ validRslt r

  assertBool "1" $ isLeft $ R.deleteIfUnused 5 D.rslt
  assertBool "addrToRefExpr of deleted" $ isLeft $ addrToRefExpr r 5
  assertBool "refExprToAddr missing"    $ isLeft $
    either error (refExprToAddr r) (addrToRefExpr D.rslt 5)
  assertBool "variety missing"   $ isLeft $ variety r 5
  assertBool "has missing"       $ isLeft $ has r 5
  assertBool "isIn missing"      $ isLeft $ isIn r 5
  assertBool "isIn $ former member of missing" $
    isIn r 1 == Right ( S.fromList [ (RoleInRel' $ RoleMember 1, 6)
                                   , (RoleInRel' $ RoleMember 2, 6) ] )
  assertBool "isIn $ another former member of missing" $
    isIn r 2 == Right S.empty

test_insert :: Test
test_insert = TestCase $ do
  let r2 = either error id
           $ R.insertAt 7 (Rel' $ Rel [1,1] 4) D.rslt
  assertBool "valid 1" $ isRight $ validRslt r2

  assertBool "1" $ isIn r2 4 == Right
    (S.fromList [ (RoleInRel' $ RoleTplt    , 7     )
                , (RoleInRel' $ RoleTplt    , 6     )
                , (RoleInRel' $ RoleTplt    , 5     ) ] )
  assertBool "2" $ isIn r2 1 == Right
    (S.fromList [ (RoleInRel' $ RoleMember 1, 7     )
                , (RoleInRel' $ RoleMember 2, 7     )
                , (RoleInRel' $ RoleMember 1, 5     ) ] )
  assertBool "3" $ has r2 7 == Right
    ( M.fromList [ (RoleInRel' $ RoleMember 1, 1     )
                 , (RoleInRel' $ RoleMember 2, 1     )
                 , (RoleInRel' $ RoleTplt    , 4     ) ] )
  assertBool "4" $ map (has D.rslt) [1..6] == map (has r2) [1..6]
  assertBool "5" $ isLeft $ has D.rslt  7

  assertBool "address collision" $ isLeft $
    R.insertAt 1 (Phrase' "nuyck") D.rslt
  assertBool "non-matching Tplt" $ isLeft $
    R.insertAt 1 (Rel' $ Rel [1,2,3] 4) D.rslt
  assertBool "nonexistent references" $ isLeft $
    R.insertAt 1 (Rel' $ Rel [11,22] 4) D.rslt
