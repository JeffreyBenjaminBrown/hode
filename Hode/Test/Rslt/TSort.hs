-- | PITFALL: Some of these tests are manual.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TSort where

import           Data.Either
import qualified Data.Set as S
import qualified Data.Map as M
import           Test.HUnit

import Hode.Rslt.RTypes
import Hode.Rslt.Index
import Hode.Rslt.RLookup.RConvert
import Hode.Rslt.Binary
import Hode.Rslt.Sort
import Hode.NoUI


test_module_rslt_sort :: Test
test_module_rslt_sort = TestList [
  TestLabel "test_nothingIsGreater" test_nothingIsGreater,
  TestLabel "test_allRelsInvolvingTplts" test_allRelsInvolvingTplts,
  TestLabel "test_allNormalMembers" test_allNormalMembers,
  TestLabel "test_withIsTop" test_withIsTop,
  TestLabel "test_justUnders" test_justUnders,
  TestLabel "test_kahnIterate" test_kahnIterate,
  TestLabel "test_allExprsButTpltsOrRelsUsingThem"
    test_allExprsButTpltsOrRelsUsingThem,
  TestLabel "test_kahnSort" test_kahnSort,
  TestLabel "test_kahnSort_tpltInNodes" test_kahnSort_tpltInNodes,
  TestLabel "test_partitionRelated" test_partitionRelated
  ]

test_partitionRelated :: Test
test_partitionRelated = TestCase $ do
  let tplt_a :: Rslt -> Addr
      tplt_a r = either (error "huh?") id $
                 head . S.toList <$>
                 nFindAddrs r "/t /_ a /_"
      stringElt :: Rslt -> String -> Addr
      stringElt r = either (error "not in graph") id .
                    exprToAddr r . Phrase

  let Right rLine = nInserts (mkRslt mempty)
        [ "x"
        , "0 #a 1", "1 #a 2", "2 #a 3"
        , "y" ]
      Right (conn,isol) =
        partitionRelated rLine (tplt_a rLine)
        $ map (stringElt rLine) ["0","1","2","3","x","y"]
  assertBool "x and y are related" $
    S.fromList isol ==
    S.fromList (map (stringElt rLine) ["x","y"])
  assertBool "the numbers are connected" $
    S.fromList conn ==
    S.fromList (map (stringElt rLine . show) [0..3::Int])

  let Right rLine' = -- the same graph, built in another order
        nInserts (mkRslt mempty)
        [ "1 #a 2"
        , "0 #a 1", "y", "2 #a 3"
        , "x" ]
      Right (conn',isol') =
        partitionRelated rLine' (tplt_a rLine')
        $ map (stringElt rLine') ["0","1","2","3","x","y"]
  assertBool "x and y are related, again" $
    S.fromList isol' ==
    S.fromList (map (stringElt rLine') ["x","y"])
  assertBool "the numbers are connected, again" $
    S.fromList conn' ==
    S.fromList (map (stringElt rLine' . show) [0..3::Int])

-- | Can I sort a set of nodes when the `Tplt`
-- being sorted by is among them?
test_kahnSort_tpltInNodes :: Test
test_kahnSort_tpltInNodes = TestCase $ do
  let ts = "(/t /_ \"\" /_)" -- template string
      Right r = nInserts (mkRslt mempty)
                [ "12 # 11"
                , "11 # " ++ ts
                , ts ++ " # 9"
                , "8 # 9" ]
      t :: Addr = either (error "wut?") id $
          head . S.toList <$>
          nFindAddrs r ts
      i :: Int -> Addr
      i = either (error "not in graph") id .
          exprToAddr r . Phrase . show
  assertBool "is Right" $ isRight $
    kahnSort r (LeftFirst,t) [t, i 9, i 11]
  assertBool "is what it should be" $
    kahnSort r (LeftFirst,t) [t, i 9, i 11]
    == Right ([i 11, t, i 9], [])

test_kahnSort :: Test
test_kahnSort = TestCase $ do
  let tplt_a :: Rslt -> Addr
      tplt_a r = either (error "wut?") id $
                 head . S.toList <$>
                 nFindAddrs r "/t /_ a /_"
      intElt :: Rslt -> Int -> Addr
      intElt r = either (error "not in graph") id .
                 exprToAddr r . Phrase . show

      ag :: Rslt -> BinOrientation -> [Addr] -> [Addr] -> [Addr] -> Bool
      ag r bo nodes sorted isol =
        let Right (s,i) = kahnSort r (bo, tplt_a r)
                          (map (intElt r) nodes)
        in s == map (intElt r) sorted &&
           S.fromList i == S.fromList (map (intElt r) isol)

  let Right r = nInserts (mkRslt mempty)
        [ "0 #a 1", "1 #a 2", "2 #a 3" ]
    in assertBool "meh" ( ag r RightFirst [0..3]  [3,2,1,0] [] ) >>
       assertBool "meh" ( ag r RightFirst [0,1,3] [3,1,0]   [] )

  let Right r = nInserts (mkRslt mempty)
        [ "0 #a 1", "1 #a 2", "2 #a 3", "1000 #meh 1001" ]
      a_isol = [1000,1001]
        -- 1000 and 1001 are not `a`-related to any other expression
    in ( assertBool "meh" $ ag r RightFirst
         (a_isol ++ [0..3]) [3,2,1,0] a_isol ) >>
       ( assertBool "meh" $ ag r RightFirst
         (a_isol ++ [3,2,0]) [3,2,0] a_isol )

  let Right rTree = nInserts (mkRslt mempty)
                    [ "0 #b 00", -- the prefix relationship
                      "0 #b 01",
                      "01 #b 010",
                      "01 #b 011",
                      "00 #b 000",
                      "00 #b 001" ]
      elt :: String -> Addr
      elt = either (error "not in graph") id .
            exprToAddr rTree . Phrase
      Right tplt_b = head . S.toList <$>
                     nFindAddrs rTree "/t /_ b /_"

  assertBool "sort a tree" $ let
    Right (sorted :: [Addr], isol :: [Addr]) =
      kahnSort rTree (RightFirst,tplt_b) $
      map elt ["0","00","01","000","001","010","011"]
    Right (shown :: [Expr]) =
      mapM (addrToExpr rTree) sorted
    in shown == map Phrase ["011","010","01",
                            "001","000","00","0"]
       && isol == []

-- | Without graph isomorphism, this test is too brittle to automate.
test_restrictRsltForSort :: IO () -- manual test
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

-- | For testing what it does to the `Rslt`
-- (as opposed to the tops) see `test_kahnIterate'`, immediately below.
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
    ek = kahnIterate (RightFirst,tplt_a) $
         Kahn r [expr 0] []
    in case ek of
         Left s                -> error s
         Right (Kahn _ tops _) -> tops == [expr 1]

-- | This lets you see the `Rslt`. (With `Rslt` isomorphisms,
-- I could automate this nicely.)
--
-- > Right (Kahn r b c) = test_kahnIterate'
-- > nShowRsltIO r
test_kahnIterate' :: Either String Kahn -- manual test
test_kahnIterate' =

  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "0 #a 2",
                                           "1 #a 2" ]
      Right tplt_a = head . S.toList <$>
                     nFindAddrs r "/t /_ a /_"
      expr :: Int -> Addr
      expr k = either
        (const $ error $ show k ++ " not in the Rslt") id
        $ head . S.toList <$> nFindAddrs r (show k)
  in kahnIterate (RightFirst,tplt_a) $
     Kahn r [expr 0] []

-- | Without graph isomorphisms, must test by hand.
-- The input integer is the Expr that gets deleted.
test_deleteHostsThenDelete :: Int -> IO () -- manual test
test_deleteHostsThenDelete i = do
  let Right r = nInserts (mkRslt mempty) [ "0 #a 1",
                                           "0 #a 2",
                                           "1 #b 2",
                                           "3 #b 3",
                                           "3" ]
      Right tplt_a = head . S.toList <$> nFindAddrs r "/t /_ a /_"
      n :: Int -> Addr
      n k = either (const $ error "not in the Rslt") id $
            head . S.toList <$> nFindAddrs r (show k)
  case deleteHostsThenDelete (n i) tplt_a r of
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
    $ justUnders (RightFirst, tplt_a) r n0
    == Right (S.fromList $ [n1,n2])

  assertBool "nothing is under 0, for (smaller #a bigger)"
    $ justUnders (LeftFirst, tplt_a) r n0
    == Right mempty

  assertBool "nothing is under 3, in any sense"
    $ [ justUnders (RightFirst, tplt_a) r n3,
        justUnders (LeftFirst,tplt_a) r n3,
        justUnders (RightFirst, tplt_b) r n3,
        justUnders (LeftFirst,tplt_b) r n3 ]
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
        allTops r (RightFirst,tplt_a) [n0,n1,n2,n3] )
    == Right (S.fromList [n0,n3])

  assertBool "2 and 3 are top w/r/t _ #a _ if right is bigger"
    $ ( S.fromList <$>
        allTops r (LeftFirst,tplt_a) [n0,n1,n2,n3] )
    == Right (S.fromList [n2,n3])

  assertBool "0, 1 and 3 are top w/r/t _ #b _ if left is bigger"
    $ ( S.fromList <$>
        allTops r (RightFirst,tplt_b) [n0,n1,n2,n3] )
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
    isTop r (RightFirst, t) number_0
  assertBool "If right is bigger, it's not." $ Right False ==
    isTop r (LeftFirst, t) number_0

  assertBool "If left is bigger, 2 is not top." $ Right False ==
    isTop r (RightFirst, t) number_2
  assertBool "If right is bigger, then it is." $ Right True ==
    isTop r (LeftFirst, t) number_2

  assertBool "1 is not top under either orientation." $ Right False ==
    isTop r (RightFirst, t) number_1
  assertBool "Ditto." $ Right False ==
    isTop r (LeftFirst, t) number_1
