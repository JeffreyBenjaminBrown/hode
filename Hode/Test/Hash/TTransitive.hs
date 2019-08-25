{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Test.Hash.TTransitive where

--import qualified Data.Map as M
import qualified Data.Set as S
import           Test.HUnit

import Hode.Hash.HToRslt
import Hode.Hash.HTypes
import Hode.Hash.Transitive
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes


test_module_rslt_connectivity :: Test
test_module_rslt_connectivity = TestList [
    TestLabel "test_transitiveRels" test_transitiveRels,
    TestLabel "test_reachable" test_reachable
    ]

test_transitiveRels :: Test
test_transitiveRels = TestCase $ do
  let Right a  = exprToAddr r $ Phrase "a"
      Right b  = exprToAddr r $ Phrase "b"
      Right b1 = exprToAddr r $ Phrase "b1"
      Right b2 = exprToAddr r $ Phrase "b2"
      Right x  = exprToAddr r $ Phrase "x"
      -- Right c  = exprToAddr r $ Phrase "c"
      -- Right d  = exprToAddr r $ Phrase "d"
      Right t  = exprToAddr r $ ExprTplt $
                 map Phrase [ "", "", "" ]

      Right (r :: Rslt) = stringHExprsToRslt
                          [ "a # b"
                          , "b # b1"
                          , "b # b2"
                          , "x # b2"
                          , "x # b3"
                          , "a # c"
                          , "d # e" ]

  assertBool "b can lead to a few places, but only b1 is allowed." $
    (S.fromList <$> transitiveRels SearchRightward r t [b1] [b])
    == Right ( S.fromList                              [(b, b1)] )
  assertBool "a gets to b, and thereby to b2. x gets to b2. They lead other places too, but only the destinations b and b2 are requested." $
    (S.fromList <$> transitiveRels SearchRightward r t [b,b2] [a,x])
    == Right ( S.fromList [(a,b), (a,b2), (x,b2) ] )


test_reachable :: Test
test_reachable = TestCase $ do
  let Right a  = exprToAddr r $ Phrase "a"
      Right b  = exprToAddr r $ Phrase "b"
      Right b1 = exprToAddr r $ Phrase "b1"
      Right b2 = exprToAddr r $ Phrase "b2"
      Right x  = exprToAddr r $ Phrase "x"
      Right c  = exprToAddr r $ Phrase "c"
      Right t  = exprToAddr r $ ExprTplt $
                 map Phrase [ "", "", "" ]

      Right (r :: Rslt) = stringHExprsToRslt
                          [ "a # b"
                          , "b # b1"
                          , "b # b2"
                          , "x # b2"
                          , "a # c"
                          , "d # e" ]

  assertBool "Leaves can reach only themselves." $
    (S.fromList <$> reachable SearchRightward r t [b1,b2,c])
    == Right (S.fromList                          [b1,b2,c])
  assertBool "1" $
    (S.fromList <$> reachable SearchRightward r t [b,x]) ==
    Right (S.fromList                             [b,x,b1,b2])
  assertBool "2" $
    (S.fromList <$> reachable SearchRightward r t [a]) ==
    Right (S.fromList                             [a,b,b1,b2,c])
  assertBool "3" $
    (S.fromList <$> reachable SearchLeftward r t  [b2]) ==
    Right (S.fromList                             [b2,x,b,a])
