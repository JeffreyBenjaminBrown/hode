{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TSort where

import qualified Data.Set as S
import qualified Data.Map as M
import           Test.HUnit

import Hode.Rslt.RTypes
import Hode.Rslt.Index
import Hode.Rslt.RLookup.RConvert
import Hode.Rslt.Binary
import Hode.Rslt.Sort
import Hode.UI.NoUI


test_module_rslt_sort :: Test
test_module_rslt_sort = TestList [
  TestLabel "test_nothingIsGreater" test_nothingIsGreater,
  TestLabel "test_allRelsInvolvingTplts" test_allRelsInvolvingTplts,
  TestLabel "test_allNormalMembers" test_allNormalMembers,
  TestLabel "test_withIsTop" test_withIsTop,
  TestLabel "test_justUnders" test_justUnders,
  TestLabel "test_kahnIterate" test_kahnIterate,
  TestLabel "test_allExprsButTpltsOrRelsUsingThem"
    test_allExprsButTpltsOrRelsUsingThem
--  TestLabel "test_kahnSort" test_kahnSort
  ]

test_kahnSort :: IO ()
test_kahnSort = do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "1 #a 2",
                                           "2 #a 3" ]
      Right tplt_a  = head . S.toList <$>
                      nFindAddrs r "/t /_ a /_"
      elt :: Int -> Addr
      elt = either (error "not in graph") id .
            exprToAddr r . Phrase . show
  putStrLn . show $
    kahnSort r (LeftIsBigger,tplt_a) (map elt [0..3])

-- | Without graph isomorphism, this test is too brittle to automate.
test_restrictRsltForSort :: IO ()
test_restrictRsltForSort = do
  let Right r = nInserts (mkRslt mempty) [ "0 #a  1",
                                           "2 #aa 3" ]
      Right tplt_a  = head . S.toList <$> nFindAddrs r "/t /_ a /_"
      Right tplt_aa = head . S.toList <$> nFindAddrs r "/t /_ aa /_"
  putStrLn "the full Rslt: "
  mapM_ (putStrLn . show) $ M.toList $ _addrToRefExpr r
  putStrLn "the restricted Rslt: "
  case restrictRsltForSort [] [tplt_a,tplt_aa] r of
    Left s   -> putStrLn s
    Right r1 -> mapM_ (putStrLn . show) $
                M.toList $ _addrToRefExpr r1

test_allExprsButTpltsOrRelsUsingThem :: Test
test_allExprsButTpltsOrRelsUsingThem = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 #a  1",
                                           "1 #a  2",
                                           "2 #aa 3" ]
      expr :: Int -> Addr
      expr k = either
        (const $ error $ show k ++ " not in the Rslt") id
        $ head . S.toList <$> nFindAddrs r (show k)
      Right tplt_a  = head . S.toList <$> nFindAddrs r "/t /_ a /_"
      Right tplt_aa = head . S.toList <$> nFindAddrs r "/t /_ aa /_"
      ts = [tplt_a,tplt_aa]
      Right r1 = restrictRsltForSort [] ts r
  assertBool "" $
    allExprsButTpltsOrRelsUsingThem r1 ts ==
    Right (S.fromList $ map expr [0..3])

test_kahnIterate :: Test
test_kahnIterate = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "0 #a 2",
                                           "1 #a 2" ]
      expr :: Int -> Addr
      expr k = either
        (const $ error $ show k ++ " not in the Rslt") id
        $ head . S.toList <$> nFindAddrs r (show k)
      Right tplt_a = head . S.toList <$> nFindAddrs r "/t /_ a /_"
  assertBool "" $ let
    ek = kahnIterate (LeftIsBigger,tplt_a) $
         Kahn r [expr 0] []
    in case ek of
         Left s                -> error s
         Right (Kahn _ tops _) -> tops == [expr 1]

-- | Without graph isomorphisms, must test by hand.
-- The input integer is the Expr that gets deleted.
test_deleteHostsThenDelete :: Int -> IO ()
test_deleteHostsThenDelete i = do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "0 #a 2",
                                           "1 #b 2",
                                           "3 #b 3",
                                           "3" ]
      n :: Int -> Addr
      n k = either (const $ error "not in the Rslt") id $
            head . S.toList <$> nFindAddrs r (show k)
  case deleteHostsThenDelete (n i) r of
    Left s -> putStrLn s
    Right res -> mapM_ (putStrLn . show) $
                 M.toList $ _addrToRefExpr res

test_justUnders :: Test
test_justUnders = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "0 #a 2",
                                           "1 #b 2",
                                           "3" ]

      Right tplt_a = head . S.toList <$> nFindAddrs r "/t /_ a /_"
      Right tplt_b = head . S.toList <$> nFindAddrs r "/t /_ b /_"
      Right n0  = head . S.toList <$> nFindAddrs r "0"
      Right n1  = head . S.toList <$> nFindAddrs r "1"
      Right n2  = head . S.toList <$> nFindAddrs r "2"
      Right n3  = head . S.toList <$> nFindAddrs r "3"

  assertBool "just under 0 are 1 and 2, for (bigger #a smaller)"
    $ justUnders (LeftIsBigger, tplt_a) r n0
    == Right (S.fromList $ [n1,n2])

  assertBool "nothing is under 0, for (smaller #a bigger)"
    $ justUnders (RightIsBigger, tplt_a) r n0
    == Right mempty

  assertBool "nothing is under 3, in any sense"
    $ [ justUnders (LeftIsBigger, tplt_a) r n3,
        justUnders (RightIsBigger,tplt_a) r n3,
        justUnders (LeftIsBigger, tplt_b) r n3,
        justUnders (RightIsBigger,tplt_b) r n3 ]
    == replicate 4 (Right mempty)

test_withIsTop :: Test
test_withIsTop = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "1 #a 2",
                                           "1 #b 2",
                                           "3" ]
      Right tplt_a = head . S.toList <$> nFindAddrs r "/t /_ a /_"
      Right tplt_b = head . S.toList <$> nFindAddrs r "/t /_ b /_"
      Right n0  = head . S.toList <$> nFindAddrs r "0"
      Right n1  = head . S.toList <$> nFindAddrs r "1"
      Right n2  = head . S.toList <$> nFindAddrs r "2"
      Right n3  = head . S.toList <$> nFindAddrs r "3"

  assertBool "0 and 3 are top w/r/t _ #a _ if left is bigger"
    $ ( S.fromList <$>
        allTops r (LeftIsBigger,tplt_a) [n0,n1,n2,n3] )
    == Right (S.fromList [n0,n3])

  assertBool "2 and 3 are top w/r/t _ #a _ if right is bigger"
    $ ( S.fromList <$>
        allTops r (RightIsBigger,tplt_a) [n0,n1,n2,n3] )
    == Right (S.fromList [n2,n3])

  assertBool "0, 1 and 3 are top w/r/t _ #b _ if left is bigger"
    $ ( S.fromList <$>
        allTops r (LeftIsBigger,tplt_b) [n0,n1,n2,n3] )
    == Right (S.fromList [n0,n1,n3])

test_allNormalMembers :: Test
test_allNormalMembers = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "1 #b 2",
                                           "2 #b 3" ]
      Right rel_01 = head . S.toList <$> nFindAddrs r "0 #a 1"
      Right rel_12 = head . S.toList <$> nFindAddrs r "1 #b 2"
      Right num_0  = head . S.toList <$> nFindAddrs r "0"
      Right num_1  = head . S.toList <$> nFindAddrs r "1"
      Right num_2  = head . S.toList <$> nFindAddrs r "2"
  assertBool "all normal members of 0 #a 1"
    $ (S.fromList <$> allNormalMembers r [rel_01])
    == Right (S.fromList [num_0, num_1])
  assertBool "all normal members of (0 #a 1) and (1 #b 2)"
    $ (S.fromList <$> allNormalMembers r [rel_01, rel_12])
    == Right (S.fromList [num_0, num_1, num_2])

test_allRelsInvolvingTplts :: Test
test_allRelsInvolvingTplts = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "2 #b 3",
                                           "4 #b 5" ]
      Right tplt_a = head . S.toList <$> nFindAddrs r "/t /_ a /_"
      Right tplt_b = head . S.toList <$> nFindAddrs r "/t /_ b /_"
      Right rel_a  = head . S.toList <$> nFindAddrs r "0 #a 1"
      Right rel_b1 = head . S.toList <$> nFindAddrs r "2 #b 3"
      Right rel_b2 = head . S.toList <$> nFindAddrs r "4 #b 5"

  assertBool "all rels involving _ #b _" $
    allRelsInvolvingTplts r [tplt_b] ==
    Right (S.fromList [rel_b1, rel_b2])
  assertBool "all rels involving _ (#a|#b) _" $
    allRelsInvolvingTplts r [tplt_a,tplt_b] ==
    Right (S.fromList [rel_a, rel_b1, rel_b2])

test_nothingIsGreater :: Test
test_nothingIsGreater = TestCase $ do
  let Right r = nInserts (mkRslt mempty) [ "0 # 1", "1 # 2" ]
      Right t = head . S.toList <$> nFindAddrs r "/t /_ \"\" /_"
      Right number_0 = head . S.toList <$> nFindAddrs r "0"
      Right number_1 = head . S.toList <$> nFindAddrs r "1"
      Right number_2 = head . S.toList <$> nFindAddrs r "2"

  assertBool "If left is bigger, 0 is top." $ Right True ==
    isTop r (LeftIsBigger, t) number_0
  assertBool "If right is bigger, it's not." $ Right False ==
    isTop r (RightIsBigger, t) number_0

  assertBool "If left is bigger, 2 is not top." $ Right False ==
    isTop r (LeftIsBigger, t) number_2
  assertBool "If right is bigger, then it is." $ Right True ==
    isTop r (RightIsBigger, t) number_2

  assertBool "1 is not top under either orientation." $ Right False ==
    isTop r (LeftIsBigger, t) number_1
  assertBool "Ditto." $ Right False ==
    isTop r (RightIsBigger, t) number_1
