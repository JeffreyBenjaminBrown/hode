{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hode.Test.Hash.TTransitive where

import qualified Data.Set as S
import           Test.HUnit

import Hode.Hash.Lookup
import Hode.Hash.Types
import Hode.Hash.Util (hor)
import Hode.Rslt.Index (mkRslt)
import Hode.Rslt.Lookup
import Hode.Rslt.Binary
import Hode.Rslt.Types
import Hode.NoUI


test_module_hash_hlookup_transitive :: Test
test_module_hash_hlookup_transitive = TestList [
    TestLabel "test_hLookup_hTrans" test_hLookup_hTrans
  , TestLabel "test_hLookup_hReach" test_hLookup_hReach
  , TestLabel "test_transitiveClsoure" test_transitiveClsoure
  , TestLabel "test_transitiveRels" test_transitiveRels
  , TestLabel "test_reachable" test_reachable
  , TestLabel "test_cyclesInvolving" test_cyclesInvolving
  , TestLabel "test_connections" test_connections
  ]

test_connections :: Test
test_connections = TestCase $ do
  let a i = either (error "absent") id
            $ exprToAddr r $ Phrase $ show i
      Right ta  = exprToAddr r $ ExprTplt $ Tplt
                  Nothing [Phrase "a"] Nothing
      Right tb  = exprToAddr r $ ExprTplt $ Tplt
                  Nothing [Phrase "b"] Nothing
      Right (r :: Rslt) = nInserts (mkRslt mempty)
        [ "1  #a 2"
        , "2  #a 3"
        , "1  #b 10"
        , "10 #b 11"
        , "1  #b 20"
        , "20 #b 21" ]
  assertBool "" $ Right [] ==
    connections r SearchRightward ta [a 2]
    (S.fromList $ map a [1,10,11])
  assertBool "" $ Right [ map a [1,2,3] ] ==
    connections r SearchRightward ta [a 1]
    (S.singleton $ a 3)
  assertBool "" $ Right [map a [1,10]] ==
    connections r SearchRightward tb [a 1]
    (S.fromList $ map a [10,11])
  assertBool "" $
    ( S.fromList <$> Right (map (map a) $
                            [ [1,10]
                            , [1,20,21] ] ) ) ==
    ( S.fromList <$> connections r SearchRightward tb
      [a 1] (S.fromList $ map a [10,11,21]) )

test_cyclesInvolving :: Test
test_cyclesInvolving = TestCase $ do
  let a i = either (error "absent") id
            $ exprToAddr r $ Phrase $ show i
      Right t  = exprToAddr r $ ExprTplt $ Tplt
                 Nothing [Phrase ""] Nothing
      Right (r :: Rslt) = nInserts (mkRslt mempty)
                            [ "-1 # 0"
                            , "0 # 1"
                            , "1 # 2"
                            , "2 # 3"
                            , "3 # 1"  -- anomalous, makes a cycle
                            , "3 # 4"
                            , "4 # 5" ]

  assertBool "1" $ cyclesInvolving r SearchRightward t (a 1)
    == Right [(t, map a [1,2,3,1])]
  assertBool "2" $ cyclesInvolving r SearchLeftward t (a 1)
    == Right [(t, map a [1,3,2,1])]
  assertBool "3" $ cyclesInvolving r SearchRightward t (a 3)
    == Right [(t, map a [3,1,2,3])]
  assertBool "4" $ cyclesInvolving r SearchLeftward t (a 3)
    == Right [(t,map a [3,2,1,3])]
  assertBool "5" $ cyclesInvolving r SearchRightward t (a 4)
    == Right []
  assertBool "6" $ cyclesInvolving r SearchLeftward t (a 0)
    == Right []

test_hLookup_hTrans :: Test
test_hLookup_hTrans = TestCase $ do
  let Right a0 = exprToAddr r $ Phrase "a0"
      -- Right a1 = exprToAddr r $ Phrase "a1"
      Right a2 = exprToAddr r $ Phrase "a2"
      Right b0 = exprToAddr r $ Phrase "b0"
      -- Right b1 = exprToAddr r $ Phrase "b1"
      Right b2 = exprToAddr r $ Phrase "b2"
      Right t  = exprToAddr r $ ExprTplt $ Tplt
                 Nothing [Phrase ""] Nothing
      
      Right (r :: Rslt) = nInserts (mkRslt mempty)
                          [ "a0 # a1"
                          , "a1 # a2"
                          , "b0 # b1"
                          , "b1 # b2" ]

  assertBool "a0 can only get to a2" $ hExprToAddrs r mempty
    ( HTrans SearchRightward [SearchRightward] (HExpr $ ExprAddr t)
      (hor [a2,b2])
      (hor [a0]) )
    == Right (S.fromList [a2])
  assertBool "only b0 can get to b2" $ hExprToAddrs r mempty
    ( HTrans SearchRightward [SearchLeftward] (HExpr $ ExprAddr t)
      (hor [b2])
      (hor [a0,b0]) )
    == Right (S.fromList [b0])
  assertBool "a0 cannot reach b2" $ hExprToAddrs r mempty
    ( HTrans SearchRightward [SearchLeftward, SearchRightward]
      (HExpr $ ExprAddr t)
      (hor [b2])
      (hor [a0]) )
    == Right S.empty

test_hLookup_hReach :: Test
test_hLookup_hReach = TestCase $ do
  let Right n0 = exprToAddr r $ Phrase "0"
      Right n1 = exprToAddr r $ Phrase "1"
      Right n2 = exprToAddr r $ Phrase "2"
      Right n3 = exprToAddr r $ Phrase "3"
      Right t  = exprToAddr r $ ExprTplt $ Tplt
                 Nothing [Phrase "lte"] Nothing

      Right (r :: Rslt) = nInserts (mkRslt mempty)
                          [ "0 #lte 1"
                          , "1 #lte 2"
                          , "2 #lte 3" ]

  assertBool "right from (greater than or equal to) 1" $
    hExprToAddrs r mempty
      (HReach SearchRightward (HExpr $ ExprAddr t) (HExpr $ ExprAddr n1) )
    == Right (S.fromList [n1,n2,n3])
  assertBool "left from (less than or equal to) 1" $
    hExprToAddrs r mempty
      (HReach SearchLeftward (HExpr $ ExprAddr t) (HExpr $ ExprAddr n1) )
    == Right (S.fromList [n1,n0])


test_transitiveClsoure :: Test
test_transitiveClsoure = TestCase $ do
  let Right n0 = exprToAddr r $ Phrase "0"
      Right n1 = exprToAddr r $ Phrase "1"
      Right n2 = exprToAddr r $ Phrase "2"
      Right n3 = exprToAddr r $ Phrase "3"
      Right t  = exprToAddr r $ ExprTplt $ Tplt
                 Nothing [Phrase "lte"] Nothing

      Right (r :: Rslt) = nInserts (mkRslt mempty)
                          [ "0 #lte 1"
                          , "1 #lte 2"
                          , "2 #lte 3" ]

  assertBool "1" $
    (S.fromList <$>
     transitiveClosure SearchRightward r [t] [n0,n1,n2,n3])
    == Right ( S.fromList [ (n0,n0), (n0,n1), (n0,n2), (n0,n3),
                            (n1,n1), (n1,n2), (n1,n3),
                            (n2,n2), (n2,n3),
                            (n3,n3) ] )

test_transitiveRels :: Test
test_transitiveRels = TestCase $ do
  let Right a  = exprToAddr r $ Phrase "a"
      Right b  = exprToAddr r $ Phrase "b"
      Right b1 = exprToAddr r $ Phrase "b1"
      Right b2 = exprToAddr r $ Phrase "b2"
      Right x  = exprToAddr r $ Phrase "x"
      -- Right c  = exprToAddr r $ Phrase "c"
      -- Right d  = exprToAddr r $ Phrase "d"
      Right t  = exprToAddr r $ ExprTplt $ Tplt
                 Nothing [Phrase ""] Nothing

      Right (r :: Rslt) = nInserts (mkRslt mempty)
                          [ "a # b"
                          , "b # b1"
                          , "b # b2"
                          , "x # b2"
                          , "x # b3"
                          , "a # c"
                          , "d # e" ]

  assertBool "b can lead to a few places, but only b1 is allowed." $
    (S.fromList <$> transitiveRels SearchRightward r [t] [b1] [b])
    == Right ( S.fromList                                [(b, b1)] )
  assertBool "a gets to b, and thereby to b2. x gets to b2. They lead other places too, but only the destinations b and b2 are requested." $
    (S.fromList <$> transitiveRels SearchRightward r [t] [b,b2] [a,x])
    == Right ( S.fromList [(a,b), (a,b2), (x,b2) ] )


test_reachable :: Test
test_reachable = TestCase $ do
  let Right a  = exprToAddr r $ Phrase "a"
      Right b  = exprToAddr r $ Phrase "b"
      Right b1 = exprToAddr r $ Phrase "b1"
      Right b2 = exprToAddr r $ Phrase "b2"
      Right x  = exprToAddr r $ Phrase "x"
      Right c  = exprToAddr r $ Phrase "c"
      Right t  = exprToAddr r $ ExprTplt $ Tplt
                 Nothing [Phrase ""] Nothing

      Right (r :: Rslt) = nInserts (mkRslt mempty)
                          [ "a # b"
                          , "b # b1"
                          , "b # b2"
                          , "x # b2"
                          , "a # c"
                          , "d # e" ]

  assertBool "Leaves can reach only themselves." $
    (S.fromList <$> reachable SearchRightward r [t] [b1,b2,c])
    == Right (S.fromList                            [b1,b2,c])
  assertBool "1" $
    (S.fromList <$> reachable SearchRightward r [t] [b,x]) ==
    Right (S.fromList                               [b,x,b1,b2])
  assertBool "2" $
    (S.fromList <$> reachable SearchRightward r [t] [a]) ==
    Right (S.fromList                               [a,b,b1,b2,c])
  assertBool "3" $
    (S.fromList <$> reachable SearchLeftward r [t] [b2]) ==
    Right (S.fromList                              [b2,x,b,a])
