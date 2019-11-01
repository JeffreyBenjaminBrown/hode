{-# LANGUAGE
ScopedTypeVariables #-}

module Hode.Util.Alternation (
    zip'            -- ^ [a] -> [a] -> [a]
  , tpltFromEithers -- ^ [Either a b] -> Tplt b
  ) where

import Data.Either
import Hode.Rslt.RTypes


-- | Like zip. If the first list has one more element than the second,
-- all elements will still be used.
zip' :: [a] -> [a] -> [a]
zip' (a:as) (b:bs) = a : b : zip' as bs
zip' (a:_) [] = [a]
zip' [] _ = []

-- | PITFALL: Assumes, without enforcing,
-- that Lefts and Rights alternate.
tpltFromEithers :: forall a b. [Either a b] -> Tplt b
tpltFromEithers es0 =
  let fr = fromRight (error "impossible")
      (capLeft, es1) :: (Maybe b, [Either a b]) =
        if isRight $ head es0
        then (Just $ fr $ head es0, tail es0)
        else (Nothing, es0)
  in let (capRight, es2) :: (Maybe b, [Either a b]) =
           if isRight $ last es1
           then ( Just $ fr $ last es1,
                  reverse $ tail $ reverse es1)
           else (Nothing, es1)
     in Tplt capLeft (map fr $ filter isRight es2) capRight
