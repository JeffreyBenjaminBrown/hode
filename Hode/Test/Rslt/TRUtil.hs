{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TRUtil where

import Data.Functor.Foldable
import Test.HUnit

import Hode.Rslt.Types
import Hode.Rslt.Util
import Hode.Test.Rslt.RData as D


test_module_rslt_rutil :: Test
test_module_rslt_rutil = TestList [
    TestLabel "test_toExprWith" test_toExprWith
  , TestLabel "test_addrToExprWith" test_addrToExprWith
  ]

test_addrToExprWith :: Test
test_addrToExprWith = TestCase $ do
  assertBool "Phrase" $
    addrToExprWith D.rslt 1 == Right
    ( Fix $ EFW
      ( 1, PhraseF "dog") )

  let t = Fix $ EFW
          ( 4, ExprTpltF $ Tplt Nothing
               [ Fix $ EFW (3, PhraseF "needs") ]
               Nothing )
  assertBool "Tplt" $
    addrToExprWith D.rslt 4 == Right t
  assertBool "Rel" $
    addrToExprWith D.rslt 5 == Right
    ( Fix $ EFW
      ( 5
      , ExprRelF $ Rel
        [ Fix $ EFW (1, PhraseF "dog")
        , Fix $ EFW (2, PhraseF "oxygen") ]
        t ) )

  assertBool "Tplt with right cap" $
    addrToExprWith D.rslt_rightCapped 4 == Right
    ( Fix $ EFW
      ( 4, ExprTpltF $ Tplt Nothing
           [ Fix $ EFW (3, PhraseF "needs") ]
           $ Just $ Fix $ EFW (1, PhraseF "dog") ) )

test_toExprWith :: Test
test_toExprWith = TestCase $ do
  let a = ExprAddr 0
      f = ExprAddrF 0
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
