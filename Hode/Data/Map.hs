-- By Alexandre Esteves.
-- Will eventually be part of the containers library.
--
-- https://mail.haskell.org/pipermail/libraries/2019-July/029731.html
-- https://github.com/haskell/containers/issues/647

module Hode.Data.Map where

import Prelude hiding (lookup)
import Data.Map (Map, mapMaybe, lookup)


-- | Example:
-- > import qualified Data.Map as M
-- > compose (M.fromList [(2,22),(4,44)]) (M.fromList [(1,2),(3,4)])
-- fromList [(1,22),(3,44)]
compose :: Ord b => Map b c -> Map a b -> Map a c
compose bc ab = flip mapMaybe ab $ flip lookup bc
