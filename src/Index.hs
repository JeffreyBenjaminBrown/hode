module Index where

import           Data.Maybe (isNothing)
import qualified Data.Map as M
import qualified Data.Set as S

import Rslt
import Index.Positions
import Index.ImgLookup


-- | == Build the database

-- TODO (#strict) Evaluate `Index` completely at start of program.
index :: Files -> Index
index files = Index { addressOf       = imgLookup files
                    , variety         = variety'
                    , positionsIn     = positionsIn'
                    , positionsHeldBy = positionsHeldBy'
                    }
 where
  fps = positionsWithinAll files :: [(Addr, [(Role, Addr)])]

  variety' :: Addr -> Maybe (Expr', Arity)
  variety' = flip M.lookup varieties where
    -- (#strict) Build `varieties` completely first.
    varieties = M.map exprVariety files

  positionsIn' :: Addr -> Maybe (M.Map Role Addr)
  positionsIn' = flip M.lookup positions where
    -- (#strict) Build `positions` completely first.
    positions :: M.Map Addr (M.Map Role Addr)
    positions = M.map M.fromList $ M.fromList fps

  positionsHeldBy' :: Addr -> Maybe (S.Set (Role, Addr))
  positionsHeldBy' = flip M.lookup $ positionsHeldByAll fps
    -- (#strict) Build `positionsHeldByAll fps` completely first.


-- | == Check the database

collectionsWithAbsentAddrs :: Files -> Index -> M.Map Addr [Addr]
collectionsWithAbsentAddrs files index = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . variety index

  involved :: Expr -> [Addr]
  involved (Word _)    = error "impossible"
  involved (Tplt as)   = as
  involved (Rel as a)  = a : as
  involved (Par sas _) = map snd sas

  collections :: Files
  collections = M.filter isCollection files where
    isCollection expr = case expr of Word _ -> False
                                     _      -> True

-- TODO ? Report for each bad `Addr` the kind of problem.
relsWithoutMatchingTplts :: Files -> Index -> Files
relsWithoutMatchingTplts files index = res where
  res = M.filter (not . relMatchesTpltArity) rels

  -- PITFALL: Intentionally partial (only Rels).
  relMatchesTpltArity :: Expr -> Bool
  relMatchesTpltArity e@(Rel _ t) = case variety index t of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      Tplt' -> arity e == art
      _         -> False

  rels = M.filter isRel files where
    isRel (Rel _ _) = True
    isRel _         = False


-- | == derivable from an `Index`

-- | = deterministic search

holdsPosition :: Index -> (Role, Addr) -> Maybe Addr
holdsPosition i (r,a) = case positionsIn i a of
  Nothing -> Nothing
  Just ps -> M.lookup r ps


-- | = Non-deterministic search.
-- From Curry. Likely not translateable.

-- somethingThatHolds   :: Index -> Addr -> Addr
-- somethingThatHolds i a0
--   | Just s =:= positionsHeldBy i a0
--     & fElem (RoleMember _, a) (setRBT2list s)
--   = a where a,s free
-- 
-- somethingThatHoldsAt :: Index -> Int -> Addr -> Addr
-- somethingThatHoldsAt i pos a0
--   | Just s =:= positionsHeldBy i a0
--     & fElem (RoleMember pos, a) (setRBT2list s)
--   = a where a,s free
-- 
-- aRelUsingTemplate    :: Index -> Addr -> Addr
-- aRelUsingTemplate i a0
--   | Just s =:= positionsHeldBy i a0
--     & fElem (RoleTplt, a) (setRBT2list s)
--   = a where a,s free
