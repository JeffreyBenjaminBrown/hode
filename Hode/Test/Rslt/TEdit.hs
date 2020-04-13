{-# LANGUAGE ScopedTypeVariables
, TupleSections
#-}

module Hode.Test.Rslt.TEdit where

import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Test.HUnit

import           Hode.NoUI
import           Hode.Rslt.Binary
import qualified Hode.Rslt.Edit         as R
import qualified Hode.Rslt.Edit.Initial as R
import qualified Hode.Rslt.Edit.Replace as R
import           Hode.Rslt.Index
import           Hode.Rslt.Lookup
import           Hode.Rslt.Types
import           Hode.Rslt.Valid
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D
import           Hode.Util.Misc


test_module_rslt_edit :: Test
test_module_rslt_edit = TestList [
    TestLabel "test_insert" test_insert
  , TestLabel "test_insertChain" test_insertChain
  , TestLabel "test_deleteIfUnused" test_deleteIfUnused
  , TestLabel "test_replaceInRole" test_replaceInRole
  , TestLabel "test_replaceRefExpr" test_replaceRefExpr
  , TestLabel "test_exprToAddrInsert" test_exprToAddrInsert
  , TestLabel "test_exprToAddrInsert_withCycles"
               test_exprToAddrInsert_withCycles
  , TestLabel "test_replaceExpr" test_replaceExpr
  , TestLabel "test_renameAddr_unsafe" test_renameAddr_unsafe
  , TestLabel "test_replaceInRefExpr" test_replaceInRefExpr
  , TestLabel "test_moveRefExpr" test_moveRefExpr
  , TestLabel "test_separate" test_separate
  ]

test_separate :: Test
test_separate = TestCase $ do
  let Right (r :: Rslt) = nInserts (mkRslt mempty)
        [ "1  #a 10"
        , "2  #a 10"
        , "1  #a 11"
        , "10 #a 2" -- connections in either direction will be removed
        , "1  #a 11"
        , "1  #b 11" ]
      ai :: Int -> Addr
      ai i = either (error "absent int") id -- address of an int
             $ exprToAddr r $ Phrase $ show i
      as s = -- address of a string
        either (error "absent string") (fst . head) $ nFind r s
      t j = -- address of a template
        either (error $ "absent template: " ++ j) id
        $ exprToAddr r $ ExprTplt
        $ Tplt Nothing [Phrase j] Nothing
      low  :: [Addr] = map ai [1,2]
      high :: [Addr] = map ai [10,11]

      Right (rDivided :: Rslt) = R.separate (t "a") low high r
      shouldRemain :: [Addr] = as "1 #b 11" : as "a" : as "b" :
                               t "a" : t "b" : low ++ high
      Just (highestBuiltinKey :: Addr) =
        fst <$> M.lookupMax (_addrToRefExpr $ mkRslt mempty)
      doRemain :: [Addr] =
        filter (> highestBuiltinKey) $
        M.keys $ _addrToRefExpr rDivided

  assertBool "after disconnecting low from high on the template (_ #a _), the only non-builtin relationship left is (1 #b 11)" $
      S.fromList doRemain == S.fromList shouldRemain

test_moveRefExpr :: Test
test_moveRefExpr = TestCase $ do
  let r = mkRslt $ M.fromList
          [ (0, Phrase' "")
          , (1, Tplt'$ Tplt (Just 0) [] Nothing)
          , (2, Rel' $ Rel [1] 1) ]

  assertBool "" $ R.moveRefExpr 1 4 r ==
    Right ( mkRslt $ M.fromList
            [ (0, Phrase' "")
            , (4, Tplt'$ Tplt (Just 0) [] Nothing)
            , (2, Rel' $ Rel [4] 4) ] )

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
  let Right r = nInserts (mkRslt mempty)
                [ "0 # 1" ]
      Right a0 = fst . head <$> nFind r "0"
  assertBool "" $
    R.replaceExpr a0 (Phrase "2") r
    == ( (,[]) <$>
         nInserts (mkRslt mempty) [ "2 # 1" ] )

test_exprToAddrInsert_withCycles :: Test
test_exprToAddrInsert_withCycles = TestCase $ do
  let Right (r0 :: Rslt) = nInserts (mkRslt mempty)
        [ "0 #a 1"
        , "0 #b 1"
        , "(/t /_ a /_) #is transitive" ]
      Right a0 = fst . head <$> nFind r0 "0"
      Right a1 = fst . head <$> nFind r0 "1"
      Right ta  = fst . head <$> nFind r0 "/t /_ a /_"
  assertBool "" $
    let Right (_,_,cs) = do e <- nExpr r0 "1 #a 0"
                            R.exprToAddrInsert r0 e
    in cs == map (ta,) [[a1,a0,a1]]
  assertBool "" $
    let Right (_,_,cs) = do e <- nExpr r0 "1 #b 0"
                            R.exprToAddrInsert r0 e
    in cs == []

test_exprToAddrInsert :: Test
test_exprToAddrInsert = TestCase $ do
  assertBool "1" $ let
    Right (r,as,_) = R.exprToAddrInsert D.rslt $ ExprTplt $ Tplt
                     Nothing [ExprAddr 3] Nothing
    in r == D.rslt
       && as == [Old 4]

  assertBool "2" $ let
    Right (r,as,_) = R.exprToAddrInsert D.rslt $ ExprTplt $ Tplt
                     Nothing [ExprAddr 1] Nothing
    in r == ( either error id $
              R.insertAt 7 (Tplt' $ Tplt Nothing [1] Nothing) D.rslt )
       && as == [New 7, Old 1]

  assertBool "3" $ let
    Right (r,as,_) = R.exprToAddrInsert D.rslt $ ExprTplt $ Tplt
                     (Just $ Phrase "bar") [Phrase ""]
                     (Just $ Phrase "foo")
    in r == ( either error id
              $ R.insertAt 9 (Tplt' $ Tplt (Just 7) [0] (Just 8))
              $ either error id
              $ R.insertAt 8 (Phrase' "foo")
              $ either error id
              $ R.insertAt 7 (Phrase' "bar") D.rslt )
       && as == [New 9, New 7, Old 0, New 8]

  assertBool "5" $ let
    Right (r,as,_) =
      R.exprToAddrInsert (mkRslt mempty)
      ( ExprRel $ Rel [ ExprRel $ Rel [ Phrase "space"
                                      , Phrase "empty" ]
                        ( ExprTplt $ Tplt
                          Nothing [Phrase "is"] Nothing )
                      , Phrase "suck" ]
        ( ExprTplt $ Tplt
          (Just $ Phrase "That") [Phrase "does"] Nothing ) )
    a = unAged $ head as
    n16 :: Expr =
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
  let without_6    :: Rslt =
        mkRslt $ M.delete 6 D.refExprs
      with_new_rel :: Rslt =
        either error id
        $ R.insertAt 6 (Rel' $ Rel [1,1] 4) without_6
      r            :: Rslt =
        either error id
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

test_insertChain :: Test
test_insertChain = TestCase $ do
  let tString = "/t /_ x /_"
      Right r = nInserts emptyRslt $ tString : map show [0 .. 2::Int]
      t :: TpltAddr
      t = either (error "wut")  (fst . head) $ nFind r tString
      a :: Int -> Addr -- the `Addr` of 0,1,2 or 3
      a = either (error errMsg) (fst . head) . nFind r . show
        where errMsg = "Only nodes 0,1 and 2 are present."
  assertBool "" $
    R.insertChain (LeftEarlier,t) (map a [0..2]) r
    == nInserts r ["0 #x 1","1 #x 2"]
  assertBool "" $
    R.insertChain (RightEarlier,t) (map a [0..2]) r
    == nInserts r ["1 #x 0","2 #x 1"]

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
