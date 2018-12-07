module Search where

import           Data.Maybe (isNothing)
import qualified Data.Map as M
import qualified Data.Set as S

import Rslt
import Index


-- | = deterministic search

holdsPosition :: Index -> (Role, Addr) -> Maybe Addr
holdsPosition i (r,a) = case positionsIn i a of
  Nothing -> Nothing
  Just ps -> M.lookup r ps
