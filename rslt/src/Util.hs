module Util where

import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Maybe as Mb


-- | I believe laziness will cause this to short-ciircuit,
-- finishing before exhausting the list if possible.
hasANothing :: [Maybe a] -> Bool
hasANothing = or . map Mb.isNothing

setFromMaybe :: Maybe a -> Set a
setFromMaybe = maybe S.empty S.singleton

setFromSetOfMaybes :: Ord a => Set (Maybe a) -> Set a
setFromSetOfMaybes = S.map Mb.fromJust . S.filter (not . Mb.isNothing)
