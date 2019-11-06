{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TShow where

import           Data.Either
import           Data.Functor.Foldable
import           Test.HUnit

import           Hode.Rslt.RTypes
import           Hode.Rslt.Show
import           Hode.Rslt.Show
import           Hode.Rslt.Show.Util
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_show :: Test
test_module_rslt_show = TestList [
    TestLabel "test_hashUnlessEmptyStartOrEnd" test_hashUnlessEmptyStartOrEnd
  , TestLabel "test_eShow" test_eShow
  , TestLabel "test_parenExprAtDepth" test_parenExprAtDepth
  , TestLabel "test_eParenShow" test_eParenShow
  ]

test_eParenShow :: Test
test_eParenShow = TestCase $ do
  let f k = eParenShow k D.rslt_rightCapped
  assertBool "hi" $ f   2  (Phrase "hi") == Right "hi"
  assertBool "hi" $ f (-1) (Phrase "hi") == Right "hi"
  assertBool "rel with no tplt" $ isLeft $ f 2 $
    ExprRel $ Rel [Phrase "trees", Phrase "CO2"] $ Phrase ""

  let eat = ExprTplt $ Tplt Nothing [Phrase "eat"] Nothing
  assertBool "tplt" $ f 2 eat == Right "_ eat _"

  let tIn = ExprTplt $ Tplt Nothing [Phrase "in"] Nothing
      dog = ExprTplt $ Tplt Nothing []
            $ Just $ Phrase ", dog"
      r1  = ExprRel $ Rel [Phrase "trees", Phrase "CO2"] eat
      r2  = ExprRel $ Rel
        [ ExprRel $ Rel [Phrase "trees", Phrase "Earth"] tIn
        , Phrase "CO2" ] eat

  assertBool "depth-1 rel, max depth 2" $ f 2 r1 ==
    Right "trees #eat CO2"
  assertBool ( "depth-1 rel, max depth 0" ++
               " (the outermost layer is never wrapped)" )
    $ f 2 r1 == Right "trees #eat CO2"
  assertBool "depth-1 rel, max depth 2" $ f 2 r2 ==
    Right "trees #in Earth ##eat CO2"
  assertBool "depth-1 rel, max depth 1" $ f 1 r2 ==
    Right "(trees #in Earth) #eat CO2"
  assertBool "Rel arity 2, Tplt arity 1. (PITFALL: Whether an Expr is valid is beyond eParenShowAttrs purview; for that, use Hode.Rslt.RValid.validExpr)."
    $ isRight $ f 2 $
    ExprRel $ Rel [Phrase "trees", Phrase "CO2"] dog

-- | `test_exprFWithDepth` might make this easier to understand.
-- Currently it is stored at earlier-work/Rslt/Show/JustInCase.hs.
-- That demo code, however, surely broke when Tplt changed
-- from a synonym for [] to something more complex, during
-- commit 8d163edd7381afa8955eacfd6683ff090db4688a
-- Date:   Sat Sep 14 20:00:12 2019 -0500

test_parenExprAtDepth :: Test
test_parenExprAtDepth = TestCase $ do

  let fe0 :: Fix (ExprFWith ())
      fe :: [Fix (ExprFWith ())]
         ->  Fix (ExprFWith ())
      fe0 = Fix $ EFW ( (), AddrF 0 )
      fe ms = Fix $ EFW
        ( (), ExprRelF $ Rel ms $ fe0 )

  -- like fe, but with depth and wrappedness
  let dw0 ::                 Fix (ExprFWith (Int,Parens))
      dw :: (Int,Parens) -> [Fix (ExprFWith (Int,Parens))]
                         ->  Fix (ExprFWith (Int,Parens))
      dw0 = Fix $ EFW ((0,Naked), AddrF 0)
      dw ip ms = Fix $ EFW
        ( ip, ExprRelF $ Rel ms dw0 )

  assertBool "" $ parenExprAtDepth 2 fe0 == dw0
  assertBool "" $ parenExprAtDepth 3 fe0 == dw0
  assertBool "" $ parenExprAtDepth 2
    (fe [fe0]) ==
    dw (1,Naked) [dw0]
  assertBool "" $ parenExprAtDepth 3
    ( fe [ fe [fe0], fe0 ] ) ==
    dw (2,Naked)    [dw (1,Naked) [dw0], dw0]
  assertBool "" $ parenExprAtDepth 2 -- maxDepth 2 => InParens
    ( fe [ fe [fe0], fe0 ] ) ==
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
  assertBool "5" $ hashUnlessEmptyStartOrEnd 1 ["a","b",""]
    == ["#a","#b",""]
  assertBool "6" $ hashUnlessEmptyStartOrEnd 2 ["","b","c"]
    == ["","##b","##c"]
