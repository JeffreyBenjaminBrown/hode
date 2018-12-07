module Util where

import Data.Maybe


-- | I believe laziness will cause this to short-ciircuit,
-- finishing before exhausting the list if possible.
hasANothing :: [Maybe a] -> Bool
hasANothing = or . map isNothing
