{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.NoUI where

import qualified Data.Map as M
import qualified Data.Set as S
import Test.HUnit

import Hode.UI.NoUI
import Hode.Rslt.Index
import Hode.Rslt.RTypes


test_module_NoUI :: Test
test_module_NoUI = TestList [
  TestLabel "test_insert" test_insert
  ]

test_insert :: Test
test_insert = TestCase $ do
  let ab = "a # b"
      cd = "c # d"
      Right (r1,a1) = nInsert (mkRslt mempty) ab
      Right (r2,a2) = nInsert r1 cd
      Right (s1,b1) = nInsert (mkRslt mempty) cd
      Right (s2,b2) = nInsert s1 ab
  assertBool "" $ a1 == b1
  assertBool "" $ a2 == b2
  assertBool "" $ r2  /= s2 -- they are isomorphic but not equal
  assertBool "" ( M.size (_addrToRefExpr r2) ==
                  M.size (_addrToRefExpr s2) )
  assertBool "" ( ( S.fromList $ M.elems $ _addrToRefExpr r2) ==
                  ( S.fromList $ M.elems $ _addrToRefExpr s2) )
  assertBool "" ( Right r2 ==
                  nInserts (mkRslt mempty) [ab,cd] )
