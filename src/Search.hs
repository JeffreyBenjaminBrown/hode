{-# LANGUAGE ScopedTypeVariables #-}

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
search idx (QAnd qs) = foldl S.intersection S.empty $ map (search idx) qs
search idx (QOr qs) = foldl  S.union        S.empty $ map (search idx) qs

search idx (QHasInRole r0 q0) = selectFrom positions where
  positions :: Set (Role, Addr)
  positions = S.foldl S.union S.empty -- Since only one `Expr` fills each position, `S.foldl S.union` destroys no information.
              $ S.map (flat . positionsHeldBy idx)
              $ (search idx q0 :: Set Addr)
    where flat Nothing = S.empty
          flat (Just s) = s
  selectFrom :: Set (Role, Addr) -> Set Addr
  selectFrom = setFromSetOfMaybes . S.map f
    where f :: (Role, Addr) -> Maybe Addr
          f (r,a) = if r==r0 then Just a else Nothing

search idx (QNot _)     = error "Cannot search for a QNot."
search idx (QVariety _) = error "Cannot search for a QVariety."
search idx (QVar _)     = error "Cannot search for a QVar."
