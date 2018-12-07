-- | = For experiments

module Test.Hand
--  (
--    fElem
--  )
where

import FiniteMap
import SetRBT

import Rslt
import Test.Data
import Util


-- | = Solve for values in a list

-- | Suspends, I imagine because `elem` is rigid.
l :: Int
l | elem x [1,2]
  = x where x free

l' :: Int
l' | fElem x [1,2]
   = x where x free


-- | = HOW ? Solve for values in a map
m :: Maybe Expr
m = lookupFM testFiles x where x free

m' :: Maybe Int
m' | elem k $ keysFM fm
  = lookupFM fm k where
       k free
       fm = listToFM (<) [(1,2)]


-- | = HOW ? Solve for values in a set

s :: Int
s | elemRBT x (listToSetRBT [1,2])
  = x where x free
