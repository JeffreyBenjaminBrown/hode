{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.THToRslt (test_module_hode_hToRslt) where

import           Test.HUnit
import qualified Data.Map as M

import Hode.Rslt.Index
import Hode.Rslt.RTypes
import Hode.Hash.HToRslt


test_module_hode_hToRslt :: Test
test_module_hode_hToRslt = TestList [
  TestLabel "test_stringHExprsToRslt" test_stringHExprsToRslt
  ]

test_stringHExprsToRslt :: Test
test_stringHExprsToRslt = TestCase $ do
  let Right (r :: Rslt) = stringHExprsToRslt
        [ "a #needs b"
        , "b #needs b1"
        , "b #needs b2"
        , "a #needs c"
        , "d #needs e" ]
  assertBool "0" $ r == mkRslt
    ( M.fromList
      [ (0,Phrase' "")
      , (1,Phrase' "needs")
      , (2,Tplt' [0,1,0])
      , (3,Phrase' "a")
      , (4,Phrase' "b")
      , (5,Rel' (Rel [3,4] 2))
      , (6,Phrase' "b1")
      , (7,Rel' (Rel [4,6] 2))
      , (8,Phrase' "b2")
      , (9,Rel' (Rel [4,8] 2))
      , (10,Phrase' "c")
      , (11,Rel' (Rel [3,10] 2))
      , (12,Phrase' "d")
      , (13,Phrase' "e")
      , (14,Rel' (Rel [12,13] 2)) ] )
