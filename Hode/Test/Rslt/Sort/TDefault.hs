-- | PITFALL: Some of these tests are manual.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.Sort.TDefault where

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
  assertBool "first fix the other stuff" False

test_sortTpltsForSorting :: Test
test_sortTpltsForSorting = TestCase $ do
  let tplt_db :: String -> String
      tplt_db s = "(/t /_ " ++ s ++ " /_)"
      sb_b4 :: String
      sb_b4 = "(/t sort by /_ before /_)"
      r_db :: Rslt
      Right r_db = nInserts (mkRslt mempty)
        ["#(sort by) (/t /_ is /_) #before (/t sort by /_ before /_)"]
      addr :: Rslt -> String -> Addr
      addr r s = either (error "huh?") id $
                 head . S.toList <$>
                 nFindAddrs r s

  assertBool "kahnSort believes this data has a cycle." $
    sortTpltsForSorting r_db
    == Right (map (addr r_db) [ "/t /_ is /_"
                              , "/t sort by /_ before /_" ] )

  let precedes :: String -> String -> String
      precedes x y = "#(sort by) " ++ x ++ " #before " ++ y
      Right (r1 :: Rslt) = nInserts (mkRslt mempty)
        [ precedes (tplt_db "a") (tplt_db "b")
        , precedes               (tplt_db "b") sb_b4
        , "#(sort by) " ++ tplt_db "disconnected"
        , precedes                             sb_b4 (tplt_db "c")
        ]
  assertBool ( "This is almost working, but the tplt (_ a _) is missing.\n"
               ++ "It is present among `ts`, the `Tplt`s being sorted.\n"
               ++ "So the problem appears to be in kahnSort." )
    $ sortTpltsForSorting r1
    == Right (map (addr r1) [ tplt_db "a"
                            , tplt_db "b"
                            , sb_b4
                            , tplt_db "c"
                            , tplt_db "disconnected" ] )

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
