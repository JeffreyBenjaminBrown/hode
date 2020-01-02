-- | PITFALL: Some of these tests are manual.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.Sort.TDefault where

import qualified Data.Set as S
import           Test.HUnit

import Hode.Rslt.RTypes
import Hode.Rslt.Index
import Hode.Rslt.Sort.Default
import Hode.NoUI


test_module_rslt_sort_default :: Test
test_module_rslt_sort_default = TestList [
    TestLabel "test_sortTpltsForSorting" test_sortTpltsForSorting
  ]

tplt :: String -> String
tplt s = "(/t /_ " ++ s ++ " /_)"
sb_b4 :: String = "(/t sort by /_ before /_)"
precedes :: String -> String -> String
precedes x y = "#(sort by) " ++ x ++ " #before " ++ y
Right (r :: Rslt) = nInserts (mkRslt mempty)
  [ precedes (tplt "a") (tplt "b")
  , precedes            (tplt "b") sb_b4
  , "#(sort by) " ++ tplt "disconnected"
  , precedes                       sb_b4 (tplt "c")
  ]
tpltAddr :: String -> Addr
tpltAddr s = either (error "huh?") id $
             head . S.toList <$>
             nFindAddrs r s

test_sortTpltsForSorting :: Test
test_sortTpltsForSorting = TestCase $ do
  assertBool ( "This is almost working, but the tplt (_ a _) is missing.\n"
               ++ "It is present among `ts`, the `Tplt`s being sorted.\n"
               ++ "So the problem appears to be in kahnSort." )
    $ sortTpltsForSorting r
    == Right (map tpltAddr [ tplt "a"
                           , tplt "b"
                           , sb_b4
                           , tplt "c"
                           , tplt "disconnected" ] )
