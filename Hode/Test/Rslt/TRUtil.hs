{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TRUtil where

import Data.Functor.Foldable
import Test.HUnit

import Hode.Rslt.RTypes
import Hode.Rslt.RUtil


test_module_rslt_rutil :: Test
test_module_rslt_rutil = TestList [
    TestLabel "test_toExprWith" test_toExprWith
  ]

test_toExprWith :: Test
test_toExprWith = TestCase $ do
  let a = Addr 0
      f = AddrF 0
      fw :: Fix (ExprFWith ()) =
        Fix $ EFW ( (), f )
      e = ExprRel $ Rel
          [ ExprRel $ Rel [a, a] $ a
          , a ] $ a
      ew = Fix $ EFW ( (), ExprRelF $ Rel
                           [ Fix $ EFW ( (), ExprRelF $ Rel
                                             [ fw , fw ] $ fw )
                           , fw ] fw )
  assertBool "Attach () to every level of an Expr." $
    toExprWith () e == ew
  assertBool "Convert an ExprFWith to an Expr" $
    exprWithout ew == e
