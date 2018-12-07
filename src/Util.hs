module Util where

import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe


-- | I believe laziness will cause this to short-ciircuit,
-- finishing before exhausting the list if possible.
hasANothing :: [Maybe a] -> Bool
hasANothing = or . map isNothing

setFromMaybe :: Maybe a -> Set a
setFromMaybe = maybe S.empty S.singleton
