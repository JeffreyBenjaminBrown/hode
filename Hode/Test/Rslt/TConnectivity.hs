{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Rslt.TConnectivity where

import qualified Data.Set as S
import           Test.HUnit

import Hode.Hash.HToRslt
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.Connectivity


test_module_rslt_connectivity :: Test
test_module_rslt_connectivity = TestList [
    TestLabel "test_reachable" test_reachable
    ]

test_reachable :: Test
test_reachable = TestCase $ do
  let Right (r :: Rslt) = stringHExprsToRslt
                          [ "a # b"
                          , "b # b1"
                          , "b # b2"
                          , "x # b2"
                          , "a # c"
                          , "d # e" ]
      Right a  = exprToAddr r $ Phrase "a"
      Right b  = exprToAddr r $ Phrase "b"
      Right b1 = exprToAddr r $ Phrase "b1"
      Right b2 = exprToAddr r $ Phrase "b2"
      Right x  = exprToAddr r $ Phrase "x"
      Right c  = exprToAddr r $ Phrase "c"
      Right t  = exprToAddr r $ ExprTplt $
                 map Phrase [ "", "", "" ]
  assertBool "" $
    (S.fromList <$> rightReachable r t a) ==
    Right (S.fromList [a,b,b1,b2,c])
  assertBool "" $
    (S.fromList <$> leftReachable r t b2) ==
    Right (S.fromList [b2,x,b,a])
