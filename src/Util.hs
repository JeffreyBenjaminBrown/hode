module Util where

import Maybe
import List
import SetRBT


-- | `f` is for `flexible`
fElem :: Eq a => a -> [a] -> Bool
fElem a (a : _)  = True
fElem a (_ : as) = fElem a as
fElem _ []       = False

listToSetRBT :: Ord a => [a] -> SetRBT a
listToSetRBT = foldl (flip insertRBT) $ emptySetRBT (<)

-- | `sort (<)` sorts a list in increasing order.
sort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort ordered (a:as) = let (gt, lt) = partition (ordered a) as
                      in sort ordered lt ++ [a] ++ sort ordered gt

-- | I believe laziness will cause this to short-ciircuit,
-- finishing before exhausting the list if possible.
hasANothing :: [Maybe a] -> Bool
hasANothing = or . map isNothing
