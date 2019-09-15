module Hode.Test.Rslt.RData (
    test_the_rslt_test_data -- ^ Test
  , r0          -- ^ Rslt
  , refExprs    -- ^ Map Addr RefExpr
  , badRefExprs -- ^ Map Addr RefExpr
  , rslt        -- ^ Rslt
  , big         -- ^ Rslt
  , b2          -- ^ Rslt
  ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Test.HUnit

import Hode.Rslt.Index
import Hode.Rslt.RTypes
import Hode.Rslt.RValid


-- | local shorthand

n :: Maybe a
n = Nothing

j :: a -> Maybe a
j = Just


-- | = Make sure each Rslt is okay.

test_the_rslt_test_data :: Test
test_the_rslt_test_data = TestList [
  TestLabel "test_variety" $ TestCase $ do
      assertBool "1" $ validRslt rslt == Right ()
      assertBool "2" $ validRslt big  == Right ()
      assertBool "3" $ validRslt b2   == Right ()
  ]


-- | = the smallest `Rslt`

r0 :: Rslt
r0 = mkRslt mempty


-- | = a small `Rslt`

refExprs :: Map Addr RefExpr
refExprs = M.fromList
  [ (0, Phrase' "")
  , (1, Phrase' "dog")
  , (2, Phrase' "oxygen")
  , (3, Phrase' "needs")
  , (4, Tplt'$ Tplt n [3] n)
  , (5, Rel' $ Rel [1,2] 4)
  , (6, Rel' $ Rel [5,2] 4) -- This is a nonsense phrase.
    -- I would have to add Exprs to make it not so, which would break things.
  ]

badRefExprs :: Map Addr RefExpr
badRefExprs = M.union refExprs newData where
  newData = M.fromList [ (1001, Rel' $ Rel [1,2] 5)
                       , (1002, Rel' $ Rel [1,2] (-1000))
                       ]

rslt :: Rslt
rslt = mkRslt refExprs


-- | a big, abstract `Rslt`

big :: Rslt
big = mkRslt $ M.fromList
  [ (  0, Phrase' "0")
  , (  1, Phrase' "1")
  , (  2, Phrase' "2")
  , (  3, Phrase' "3")
  , (  4, Tplt' $ Tplt n [] n)   -- the unary rel
  , (  5, Tplt' $ Tplt n [0] n) -- the binary rel
  , (  6, Rel' $ Rel [1]   4)
  , (  7, Rel' $ Rel [1,2] 5)
  , (  8, Rel' $ Rel [6,7] 5)
  , (  9, Rel' $ Rel [2,3] 5)
  , ( 10, Rel' $ Rel [7,8] 5)
  ]


-- | = a bigger, concrete `Rslt`

b2 :: Rslt
b2 = mkRslt $ M.fromList
  [ ( 0, Phrase' "")
  , ( 1, Rel' $ Rel [2,3] 8) -- fish #like water
  , ( 2, Phrase' "fish")
  , ( 3, Phrase' "water")
  , ( 4, Phrase' "need")
  , ( 5, Phrase' "like")
  , ( 6, Phrase' "jumping")
  , ( 7, Tplt' $ Tplt n [4] n ) -- _ need _
  , ( 8, Tplt' $ Tplt n [5] n ) -- _ like _
  , ( 9, Rel' $ Rel [2,3] 7) -- fish #need water
  , (10, Rel' $ Rel [2,6] 8) -- fish #like jumping
  , (11, Phrase' "horses")
  , (12, Rel' $ Rel [11,6] 8) -- horses #like jumping
  , (13, Phrase' "exercise")
  , (14, Phrase' "is")
  , (15, Tplt' $ Tplt n [14] n ) -- _ is _
  , (16, Rel' $ Rel [6,13] 15 ) -- jumping #is exercise
  , (17, Phrase' "dolphins")
  , (18, Rel' $ Rel [17,2] 7) -- dolphins #need fish
  , (19, Rel' $ Rel [17,3] 8) -- dolphins #like water
  ]
