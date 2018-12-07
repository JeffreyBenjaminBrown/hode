{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

module Index where

import Maybe (isNothing)
import FiniteMap
import SetRBT

import Rslt
import Index.Positions
import Index.ImgLookup
import Util (fElem)


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
  variety' = lookupFM varieties where
    -- (#strict) Build `varieties` completely first.
    varieties = mapFM (\_ v -> exprVariety v) files

  positionsIn' :: Addr -> Maybe (FM Role Addr)
  positionsIn' = lookupFM positions where
    -- (#strict) Build `positions` completely first.
    positions :: FM Addr (FM Role Addr)
    positions = mapFM (\_ v -> listToFM (<) v) $ listToFM (<) fps

  positionsHeldBy' :: Addr -> Maybe (SetRBT (Role, Addr))
  positionsHeldBy' = lookupFM $ positionsHeldByAll fps
    -- (#strict) Build `positionsHeldByAll fps` completely first.


-- | == Check the database

collectionsWithAbsentAddrs :: Files -> Index -> FM Addr [Addr]
collectionsWithAbsentAddrs files index = res where
  res = filterFM (\_ v -> not $ null v)
        $ mapFM (\_ v -> filter absent $ involved v) collections

  absent :: Addr -> Bool
  absent = isNothing . variety index

  involved :: Expr -> [Addr]
  involved (Word _)          = error "impossible"
  involved (Tplt as)     = as
  involved (Rel as a)        = a : as
  involved (Par sas _) = map snd sas

  collections :: Files
  collections = filterFM (\_ v -> isCollection v) files where
    isCollection expr = case expr of Word _ -> False
                                     _      -> True

-- TODO ? Report for each bad `Addr` the kind of problem.
relsWithoutMatchingTplts :: Files -> Index -> Files
relsWithoutMatchingTplts files index = res where
  res = filterFM (\_ e -> not $ relMatchesTpltArity e) rels

  -- PITFALL: Intentionally partial (only Rels).
  relMatchesTpltArity :: Expr -> Bool
  relMatchesTpltArity e@(Rel _ t) = case variety index t of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      Tplt' -> arity e == art
      _         -> False

  rels = filterFM (\_ v -> isRel v) files where
    isRel expr = case expr of Rel _ _ -> True
                              _       -> False


-- | == derivable from an `Index`

-- | = deterministic search

holdsPosition :: Index -> (Role, Addr) -> Maybe Addr
holdsPosition i (r,a) = case positionsIn i a of
  Nothing -> Nothing
  Just ps -> lookupFM ps r


-- | = non-deterministic search

somethingThatHolds   :: Index -> Addr -> Addr
somethingThatHolds i a0
  | Just s =:= positionsHeldBy i a0
    & fElem (RoleMember _, a) (setRBT2list s)
  = a where a,s free

somethingThatHoldsAt :: Index -> Int -> Addr -> Addr
somethingThatHoldsAt i pos a0
  | Just s =:= positionsHeldBy i a0
    & fElem (RoleMember pos, a) (setRBT2list s)
  = a where a,s free

aRelUsingTemplate    :: Index -> Addr -> Addr
aRelUsingTemplate i a0
  | Just s =:= positionsHeldBy i a0
    & fElem (RoleTplt, a) (setRBT2list s)
  = a where a,s free
