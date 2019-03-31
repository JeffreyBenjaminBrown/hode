{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.TEdit where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import           Rslt.RLookup hiding (exprToAddr)
import qualified Rslt.Edit as R
import           Rslt.Index
import           Rslt.RTypes
import           Rslt.RValid
import           Rslt.Show
import qualified Test.Rslt.RData as D


test_module_rslt_edit :: Test
test_module_rslt_edit = TestList [
    TestLabel "test_insert" test_insert
  , TestLabel "test_deleteUnused" test_deleteUnused
  , TestLabel "test_replaceInRole" test_replaceInRole
  , TestLabel "test_replace" test_replace
  , TestLabel "test_exprToAddrInsert" test_exprToAddrInsert
  ]

test_exprToAddrInsert :: Test
test_exprToAddrInsert = TestCase $ do
  assertBool "1" $ R.exprToAddrInsert D.rslt ( Tplt [ Addr 0
                                                     , Addr 3
                                                     , Addr 0 ] )
    == Right (D.rslt, 4)
  assertBool "2" $ R.exprToAddrInsert D.rslt ( Tplt [ Addr 0
                                                     , Addr 1
                                                     , Addr 0 ] )
    == Right ( fromRight (error "wut") $ R.insertAt 7 (Tplt' [0,1,0]) D.rslt
             , 7 )

  assertBool "3" $ R.exprToAddrInsert D.rslt ( Tplt [ Phrase "bar"
                                                     , Phrase ""
                                                     , Phrase "foo" ] )
    == Right ( fromRight (error "wut")
               $ R.insertAt 9 (Tplt' [7,0,8])
               $ fromRight (error "wut")
               $ R.insertAt 8 (Phrase' "foo")
               $ fromRight (error "wut")
               $ R.insertAt 7 (Phrase' "bar") D.rslt
             , 9 )

  assertBool "4" $ R.exprToAddrInsert D.rslt
    ( Par [ ("The template", Tplt $ map Addr [0,3,0])
               , ("could use a", Phrase "taxi") ] "" )
    == Right ( fromRight (error "wut")
               $ R.insertAt 8 (Par' [ ("The template", 4)
                                   , ("could use a", 7) ] "")
               $ fromRight (error "wut")
               $ R.insertAt 7 (Phrase' "taxi") D.rslt
             , 8 )

  assertBool "5" $ let
    Right (r,a) = R.exprToAddrInsert D.rslt
                  ( ExprRel [ ExprRel [ Phrase "space"
                                        , Phrase "empty" ]
                               ( Tplt [ Phrase ""
                                           , Phrase "is"
                                           , Addr 0 ] )
                             , Phrase "suck" ]
                    ( Tplt [ Phrase "That"
                                , Phrase "does"
                                , Addr 0 ] ) )
    (n16 :: Expr) =
      either (error "wut") id $ addrToRefExpr r a >>= refExprToExpr r
    in eShow r n16 == Right "##That space #is empty ##does suck"

test_replace :: Test
test_replace = TestCase $ do
  assertBool "replace word in rel" $
    either (error "wut") id (R.replace (Phrase' "foo") 1 D.rslt)
    == mkRslt ( M.fromList
          [ (0, Phrase' "")
          , (2, Phrase' "oxygen")
          , (3, Phrase' "needs")
          , (4, Tplt' [0,3,0])
          , (5, Rel' [7,2] 4) -- all changes involve address 7
          , (6, Par' [("The first relationship in this graph is ", 5)] ".")
          , (7, Phrase' "foo")
          ] )

  assertBool "replace word in template" $
    either (error "wut") id (R.replace (Phrase' "foo") 0 D.rslt)
    == mkRslt ( M.fromList
         [ (7, Phrase' "foo")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (4, Tplt' [7,3,7]) -- all changes involve address 7
         , (5, Rel' [1,2] 4)
         , (6, Par' [("The first relationship in this graph is ", 5)] ".")
         ] )

  assertBool "replace rel" $
    either (error "wut") id (R.replace (Rel' [2,1] 4) 5 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (4, Tplt' [0,3,0])
         , (7, Rel' [2,1] 4) -- all changes involve address 7
         , (6, Par' [("The first relationship in this graph is ", 7)] ".")
         ] )

  assertBool "todo : replace tplt" $
    either (error "wut") id (R.replace (Tplt' [2,2,2]) 4 D.rslt)
    == mkRslt ( M.fromList
         [ (0, Phrase' "")
         , (1, Phrase' "dog")
         , (2, Phrase' "oxygen")
         , (3, Phrase' "needs")
         , (7, Tplt' [2,2,2])
         , (5, Rel' [1,2] 7) -- all changes involve address 7
         , (6, Par' [("The first relationship in this graph is ", 5)] ".")
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
  assertBool "2" $ isIn r 2 == Right S.empty
  assertBool "3" $ has r 5 == Right ( M.fromList [ (RoleMember 1, 1)
                                                 , (RoleMember 2, 1)
                                                 , (RoleTplt    , 4) ] )

  let r2 = either (error "wut") id
           $ R.replaceInRole (RoleMember 2) 8 5
           $ either (error "wut") id
           $ R.insertAt 8 (Phrase' "foo") D.rslt
  assertBool "4" $ isIn r2 8 == Right (S.singleton (RoleMember 2, 5))

test_deleteUnused :: Test
test_deleteUnused = TestCase $ do
  -- from D.rslt, remove the Par called 6 (because it uses the Rel'5)
  -- and insert at 6 (Rel' [1,1] 4), before deleting at 5 (Rel'(1,2) 4).
  -- Now 1 should be in the new rel and not the old, and 2 should be in nothing.
  let (without_6    :: Rslt) = mkRslt $ M.delete 6 D.refExprs
      (with_new_rel :: Rslt) = either (error "wut") id
                               $ R.insertAt 6 (Rel' [1,1] 4) without_6
      (r            :: Rslt) = either (error "wut") id
                               $ R.deleteUnused 5 with_new_rel
  assertBool "valid 1" $ isRight $ validRslt without_6
  assertBool "valid 2" $ isRight $ validRslt with_new_rel
  assertBool "valid 3" $ isRight $ validRslt r

  assertBool "1" $ isLeft $ R.deleteUnused 5 D.rslt
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
           $ R.insertAt 7 (Rel' [1,1] 4) D.rslt
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
    R.insertAt 1 (Phrase' "nuyck") D.rslt
  assertBool "non-matching template" $ isLeft $
    R.insertAt 1 (Rel' [1,2,3] 4) D.rslt
  assertBool "nonexistent references" $ isLeft $
    R.insertAt 1 (Rel' [11,22] 4) D.rslt
