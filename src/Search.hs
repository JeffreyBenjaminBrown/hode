module Search where

import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Rslt
import Index
import Util


-- | = deterministic search

holdsPosition :: Index -> (Role, Addr) -> Maybe Addr
holdsPosition i (r,a) = case positionsIn i a of
  Nothing -> Nothing
  Just ps -> M.lookup r ps

search :: Index -> Query -> Set Addr
search idx (QImg im) = setFromMaybe $ addrOf idx im
