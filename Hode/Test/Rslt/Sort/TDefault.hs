-- | PITFALL: Some of these tests are manual.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.Sort.TDefault where

import qualified Data.Map as M
import qualified Data.Set as S
import           Test.HUnit

import Hode.NoUI
import Hode.Rslt.Index
import Hode.Rslt.RLookup.RConvert
import Hode.Rslt.RTypes
import Hode.Rslt.Sort.Default


test_module_rslt_sort_default :: Test
test_module_rslt_sort_default = TestList [
    TestLabel "test_usesTplt" test_usesTplt
  , TestLabel "test_isIn_usingTplt" test_isIn_usingTplt
  , TestLabel "test_sortTpltsForSorting" test_sortTpltsForSorting
  , TestLabel "test_firstApplicableTplt" test_firstApplicableTplt
  ]

test_firstApplicableTplt :: Test
test_firstApplicableTplt = TestCase $ do
  let r0 :: Rslt
      Right r0 = nInserts (mkRslt mempty)
        ["#(sort by) (/t /_ is /_) #before (/t sort by /_ before /_)"]
      sb_b4 :: String
      sb_b4 = "(/t sort by /_ before /_)"
      addr :: Rslt -> String -> Addr
      addr r s = either (error "huh?") id $
                 head . S.toList <$>
                 nFindAddrs r s
      addrs :: Rslt -> [Addr]
      addrs = M.keys . _addrToRefExpr

  assertBool "where (sort by _ before _) precedes (_ is _), on all nodes" $
    firstApplicableTplt r0 (addrs r0)
    == Right (Just $ addr r0 sb_b4)
  assertBool "given nothing to sort, no template applies" $
    firstApplicableTplt r0 []
    == Right Nothing

  let Right r1 = nInserts (mkRslt mempty)
        [ "#(sort by) (/t /_ is /_)             #before (/t /_ a /_)"
        , "#(sort by) (/t /_ a /_)              #before (/t sort by /_ before /_)"
        , "#(sort by) (/t sort by /_ before /_) #before (/t /_ b /_)"
        , "a0 #a a1"
        , "b0 #b b1"
        , "c0 #c c1"
        , "ab0 #a ab1"
        , "ab0 #b ab1" ]
  assertBool "a0 and a1 are a-related" $
    firstApplicableTplt r1 (map (addr r1) ["a0","a1"])
    == Right (Just $ addr r1 "/t /_ a /_")
  assertBool "b0 and b1 are b-related" $
    firstApplicableTplt r1 (map (addr r1) ["b0","b1"])
    == Right (Just $ addr r1 "/t /_ b /_")
  assertBool "c0 and c1 are related by no template that is sorted by" $
    firstApplicableTplt r1 (map (addr r1) ["c0","c1"])
    == Right Nothing
  assertBool "ab0 and ab1 are a- and b-related, and a-relation comes first" $
    firstApplicableTplt r1 (map (addr r1) ["ab0","ab1"])
    == Right (Just $ addr r1 "/t /_ a /_")

test_sortTpltsForSorting :: Test
test_sortTpltsForSorting = TestCase $ do
  let tplt :: String -> String
      tplt s = "(/t /_ " ++ s ++ " /_)"
      sb_b4 :: String
      sb_b4 = "(/t sort by /_ before /_)"
      r0 :: Rslt
      Right r0 = nInserts (mkRslt mempty)
        ["#(sort by) (/t /_ is /_) #before (/t sort by /_ before /_)"]
      addr :: Rslt -> String -> Addr
      addr r s = either (error "huh?") id $
                 head . S.toList <$>
                 nFindAddrs r s

  assertBool "kahnSort believes this data has a cycle." $
    sortTpltsForSorting r0
    == Right (map (addr r0) [ "/t /_ is /_"
                            , "/t sort by /_ before /_" ] )

  let precedes :: String -> String -> String
      precedes x y = "#(sort by) " ++ x ++ " #before " ++ y
      Right (r1 :: Rslt) = nInserts (mkRslt mempty)
        [ precedes (tplt "a") (tplt "b")
        , precedes            (tplt "b") sb_b4
        , "#(sort by) " ++ tplt "disconnected"
        , precedes                       sb_b4 (tplt "c")
        ]
  assertBool ( "This is almost working, but the tplt (_ a _) is missing.\n"
               ++ "It is present among `ts`, the `Tplt`s being sorted.\n"
               ++ "So the problem appears to be in kahnSort." )
    $ sortTpltsForSorting r1
    == Right (map (addr r1) [ tplt "a"
                            , tplt "b"
                            , sb_b4
                            , tplt "c"
                            , tplt "disconnected" ] )

test_isIn_usingTplt :: Test
test_isIn_usingTplt = TestCase $ do
  let Right (r :: Rslt) = nInserts (mkRslt mempty)
        [ "ab #a"
        , "ab #b"
        , "bc #b"
        , "bc #c" ]
      expr s = either (error "not in graph") id .
               exprToAddr r $ Phrase s
      tplt s = either (error "not in graph") id .
               exprToAddr r $ ExprTplt $
               Tplt Nothing [] (Just $ Phrase s)
  assertBool "ab is in an a-rel" $
    isIn_usingTplt r (tplt "a") (expr "ab")
    == Right True
  assertBool "ab is in a b-rel" $
    isIn_usingTplt r (tplt "b") (expr "ab")
    == Right True
  assertBool "ab is in no c-rel" $
    isIn_usingTplt r (tplt "c") (expr "ab")
    == Right False
  assertBool "bc is in a b-rel" $
    isIn_usingTplt r (tplt "a") (expr "bc")
    == Right False
  assertBool "bc is in a c-rel" $
    isIn_usingTplt r (tplt "b") (expr "bc")
    == Right True
  assertBool "bc is in no a-rel" $
    isIn_usingTplt r (tplt "c") (expr "bc")
    == Right True

test_usesTplt :: Test
test_usesTplt = TestCase $ do
  let Right (r :: Rslt) = nInserts (mkRslt mempty)
        [ "a #a"
        , "b #b"     -- unused except to guarantee a search doesn't fail
        , "a #b c"
        , "b #c a" ] -- unused except to guarantee a search doesn't fail
      get s = fst . head $ ( either (error "not present") id
                             $ nFind r s )
  assertBool "(a #a) uses the tplt (_ a)" $ True ==
    usesTplt r (get "/t /_ a") (get "a #a")
  assertBool "(a #a) does not use the tplt (_ b)" $ False ==
    usesTplt r (get "/t /_ b") (get "a #a")
  assertBool "(a #b c) uses the tplt (_ b _)" $ True ==
    usesTplt r (get "/t /_ b /_") (get "a #b c")
  assertBool "(a #b c) does not use the tplt (_ c _)" $ False ==
    usesTplt r (get "/t /_ c /_") (get "a #b c")
