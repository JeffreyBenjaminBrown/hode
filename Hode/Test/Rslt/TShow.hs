{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TShow where

import           Data.Functor.Foldable
import           Test.HUnit

import           Hode.Rslt.RTypes
import           Hode.Rslt.RUtil
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_show :: Test
test_module_rslt_show = TestList [
    TestLabel "test_hashUnlessEmptyStartOrEnd" test_hashUnlessEmptyStartOrEnd
  , TestLabel "test_eShow" test_eShow
  , TestLabel "test_exprFWithDepth" test_exprFWithDepth
  , TestLabel "test_wrapExprAtDepth" test_wrapExprAtDepth
  ]

-- | `test_exprFWithDepth` might make this easier to understand
test_wrapExprAtDepth :: Test
test_wrapExprAtDepth = TestCase $ do

  -- fe is a Fix ExprFWith
  let fe :: [Fix (ExprFWith ())]
         ->  Fix (ExprFWith ())
      fe0 :: Fix (ExprFWith ())
      fe0 = Fix $ EFW ( (), AddrF 0 )
      fe ms = Fix $ EFW
        ( (), ExprRelF $ Rel ms $ fe0 )

  -- dw0 and dw are like fe, but with depth and wrappedness
  let dw :: (Int,Wrap) -> [Fix (ExprFWith (Int,Wrap))]
                       ->  Fix (ExprFWith (Int,Wrap))
      dw0 ::               Fix (ExprFWith (Int,Wrap))
      dw0 = Fix $ EFW ((0,Naked), AddrF 0)
      dw b rs = Fix $ EFW
        ( b, ExprRelF $ Rel rs dw0 )

  assertBool "" $ wrapExprAtDepth 2 fe0 == dw0
  assertBool "" $
    wrapExprAtDepth 2 (fe [fe0]) == dw (1,Naked) [dw0]
  assertBool "" $
    wrapExprAtDepth 2 (fe [ fe [fe0]
                          , fe0 ] ) ==
    dw (2,Wrapped) [dw (1,Naked) [dw0], dw0]

test_exprFWithDepth :: Test
test_exprFWithDepth = TestCase $ do
  let e :: Int -> Expr
      e 0 = Addr 0
      e n = ExprRel $ Rel [e $ n-1] $ e 0
      fe :: Int -> Fix (ExprFWith (Int,()))
      fe 0 = Fix $ EFW ( (0,()), AddrF 0 )
      fe n = ( Fix $
              EFW ( (n,()), ExprRelF $ Rel [fe $ n-1] $ fe 0 ) )
  assertBool "1" $
    ( exprFWithDepth $ toExprWith () $ e 1 ) == fe 1
  assertBool "1" $
    ( exprFWithDepth $ toExprWith () $ e 2 ) == fe 2

test_eShow :: Test
test_eShow = TestCase $ do
  assertBool "1" $ eShow D.rslt (Phrase "hello") == Right "hello"
  assertBool "2" $ eShow D.rslt (ExprTplt $ map Phrase ["a","b","c"] )
    == Right "a _ b _ c"
  assertBool "3" $ eShow D.rslt
    ( ExprRel ( Rel ( map Phrase ["a","b"] )
                $ ExprTplt $ map Phrase ["","=",""] ) )
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
