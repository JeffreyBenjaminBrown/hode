{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TShow where

import           Data.Functor.Foldable
import           Test.HUnit

import           Hode.Rslt.RTypes
import           Hode.Rslt.Show
import           Hode.Rslt.Show.Util
import           Hode.Rslt.Show.Wut
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_show :: Test
test_module_rslt_show = TestList [
    TestLabel "test_hashUnlessEmptyStartOrEnd" test_hashUnlessEmptyStartOrEnd
  , TestLabel "test_eShow" test_eShow
  , TestLabel "test_parenExprAtDepth" test_parenExprAtDepth
  ]

-- | `test_exprFWithDepth` might make this easier to understand.
-- Currently it is stored at earlier-work/Rslt/Show/JustInCase.hs.
-- That demo code, however, surely broke when Tplt changed
-- from a synonym for [] to something more complex, during
-- commit 8d163edd7381afa8955eacfd6683ff090db4688a
-- Date:   Sat Sep 14 20:00:12 2019 -0500

test_parenExprAtDepth :: Test
test_parenExprAtDepth = TestCase $ do

  -- fe is a Fix ExprFWith
  let fe :: [Fix (ExprFWith ())]
         ->  Fix (ExprFWith ())
      fe0 :: Fix (ExprFWith ())
      fe0 = Fix $ EFW ( (), AddrF 0 )
      fe ms = Fix $ EFW
        ( (), ExprRelF $ Rel ms $ fe0 )

  -- dw0 and dw are like fe, but with depth and wrappedness
  let dw :: (Int,Parens) -> [Fix (ExprFWith (Int,Parens))]
                         ->  Fix (ExprFWith (Int,Parens))
      dw0 ::                 Fix (ExprFWith (Int,Parens))
      dw0 = Fix $ EFW ((0,Naked), AddrF 0)
      dw b rs = Fix $ EFW
        ( b, ExprRelF $ Rel rs dw0 )

  assertBool "" $ parenExprAtDepth 2 fe0 == dw0
  assertBool "" $
    parenExprAtDepth 2 (fe [fe0]) == dw (1,Naked) [dw0]
  assertBool "" $
    parenExprAtDepth 2 (fe [ fe [fe0]
                          , fe0 ] ) ==
    dw (2,InParens) [dw (1,Naked) [dw0], dw0]

test_eShow :: Test
test_eShow = TestCase $ do
  assertBool "1" $ eShow D.rslt (Phrase "hello") == Right "hello"
  assertBool "2" $ eShow D.rslt
    (ExprTplt $ fmap Phrase $ Tplt (Just "a") ["b"] (Just "c"))
    == Right "a _ b _ c"
  assertBool "3" $ eShow D.rslt
    ( ExprRel $ Rel
      [Phrase "a", Phrase "b"]
      $ ExprTplt $ fmap Phrase $
      Tplt Nothing ["="] Nothing )
    == Right "a #= b"

test_hashUnlessEmptyStartOrEnd :: Test
test_hashUnlessEmptyStartOrEnd = TestCase $ do
  assertBool "1" $ hashUnlessEmptyStartOrEnd 2 [] == []
  assertBool "2" $ hashUnlessEmptyStartOrEnd 2 [""] == [""]
  assertBool "3" $ hashUnlessEmptyStartOrEnd 2 ["",""] == ["",""]
  assertBool "4" $ hashUnlessEmptyStartOrEnd 2 ["","",""] == ["","##",""]
  assertBool "5" $ hashUnlessEmptyStartOrEnd 2 ["a","b",""]
    == ["##a","##b",""]
  assertBool "6" $ hashUnlessEmptyStartOrEnd 2 ["","b","c"]
    == ["","##b","##c"]
