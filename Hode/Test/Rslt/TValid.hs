module Hode.Test.Rslt.TValid where

import           Data.Either
import           Test.HUnit

import           Hode.Rslt.RTypes
import           Hode.Rslt.RValid
import qualified Hode.Test.Rslt.RData as D


test_module_rslt_valid :: Test
test_module_rslt_valid = TestList [
    TestLabel "test_validRefExpr" test_validRefExpr
  , TestLabel "test_validExpr" test_validExpr
  ]

test_validExpr :: Test
test_validExpr = TestCase $ do
  let meh = error "irrelevant"
  assertBool "1" $ Right () == validExpr D.big (ExprAddr 0)
  assertBool "1" $ isLeft $    validExpr D.big (ExprAddr 100)
  assertBool "2" $ Right () == validExpr meh   (Phrase "a b c")
  assertBool "2" $ Right () == validExpr meh   (Phrase "a b c")
  assertBool "ExprRel, invalid member" $ isLeft
    $  validExpr D.big (ExprRel $ Rel [ ExprAddr 100 ] $ ExprAddr 101 )
  assertBool "ExprRel, false Tplt" $ isLeft
    $  validExpr D.big ( ExprRel $ Rel
                         [ ExprAddr 0, ExprAddr 0 ] $ ExprAddr 0)
  assertBool "ExprRel, arity mismatch" $ isLeft
    $  validExpr D.big ( ExprRel $ Rel [] $ ExprAddr 4)
  assertBool "ExprRel"                 $ Right ()
    == validExpr D.big ( ExprRel $ Rel [ExprAddr 0] $ ExprAddr 4)

test_validRefExpr :: Test
test_validRefExpr = TestCase $ do
  -- TODO : test for what kind of Left, not just whether it is Left.
  -- Could do in a future-proof manner by using enum error types rather
  -- than strings, (But I checked by hand in GHCI; each `validRefExpr ...`
  -- expression below produces the correct kind of complaint.)
  assertBool "good ExprRel" $ isRight $
    validRefExpr D.rslt (Rel' $ Rel [1,2] 4)
  assertBool "absent members" $ isLeft $
    validRefExpr D.rslt (Rel' $ Rel [100,200] 4)
  assertBool "absent Tplt" $ isLeft $
    validRefExpr D.rslt (Rel' $ Rel [1,2] 44)
  assertBool "arity mismatch" $ isLeft $
    validRefExpr D.rslt (Rel' $ Rel [] 4)
  assertBool "tplt not a tplt" $ isLeft $
    validRefExpr D.rslt (Rel' $ Rel [4] 0)
  assertBool "word" $ isRight $
    validRefExpr D.rslt (Phrase' "meh")
