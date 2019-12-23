-- PITFALL: `hExprToAddrs` might not look like it belongs here,
-- but it is mutually recursive with the others.
--
-- (`HExprToAddrs` could be moved; I would only have to
-- rewrite `immediateNeighbors` without it.)

{-# LANGUAGE ScopedTypeVariables,
TupleSections #-}

module Hode.Hash.HLookup.Transitive (
    hExprToAddrs -- ^ Rslt -> Subst Addr -> HExpr -> Either String (Set Addr)

  , transitiveClosure -- ^ :: SearchDir
                        -- -> Rslt
                        -- -> [Addr] -- ^ binary `Tplt`s to search along.
                        -- To use more than one is weird but legal.
                        -- -> [Addr] -- ^ A subset of the graph.
                        -- -> Either String [(Addr,Addr)]

  , transitiveRels -- ^ :: SearchDir
                     -- -> Rslt
                     -- -> [Addr] -- ^ binary `Tplt`s to search along.
                     -- To use more than one is weird but legal.
                     -- -> [Addr] -- ^ places to finish
                     -- -> [Addr] -- ^ places to start
                     -- -> Either String [(Addr,Addr)]

  , cyclesInvolving -- ^ Rslt -> SearchDir -> TpltAddr -> Addr
                   -- -> Either String [[Cycle]]

  , reachable -- ^ :: SearchDir
                -- -> Rslt
                -- -> [Addr] -- ^ binary `Tplt`s to search along.
                -- To use more than one is weird but legal.
                -- -> [Addr] -- ^ places to start
                -- -> Either String [Addr]
  ) where

import           Data.Either
import qualified Data.List      as L
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Hash.HTypes
import Hode.Qseq.QTypes
import Hode.Rslt.Binary
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | `hExprToAddrs` is the Hash search workhorse.

hExprToAddrs :: Rslt -> Subst Addr -> HExpr ->
                Either String (Set Addr)

hExprToAddrs r s (HMap m) =
  -- Strategy: Find every sub-Expr in m, and then find every Expr
  -- that has one of those (level-1) sub-Exprs in each specified Role.
  prefixLeft "hExprToAddrs, called on HMap:" $ do
  permissible_members :: Map Role (Set Addr) <-
    prefixLeft "computing permissible_members:"
    $ ifLefts_map $ M.map (hExprToAddrs r s) m

  let hostCandidates ::
        Role -> Set Addr -> Either String (Set Addr)
      hostCandidates role as = do
        -- `m` says the `as` are acceptable to fill `role` in
        -- some host. This returns all those hosts.
        roleHostPairs :: Set (Role, Addr) <-
          prefixLeft "at HMap / f:" $ S.unions <$>
          ifLefts_set (S.map (isIn r) as)
        Right $ S.map snd
          $ S.filter ((==) role . fst) roleHostPairs

  hcs :: Map Role (Set Addr) <-
    prefixLeft "calculating hcs:" $ ifLefts_map $
    M.mapWithKey hostCandidates permissible_members
  case null hcs of
    True  -> Right S.empty
    False -> Right $ foldl1 S.intersection $ M.elems hcs

hExprToAddrs r s (HMember h) =
  prefixLeft "hExprToAddrs, called on HMember:" $ do
  members :: Set Addr <- hExprToAddrs r s h
  hosts :: Set Addr <-
    (S.map snd) . S.unions <$>
    ifLefts_set (S.map (isIn r) members)
  Right hosts

hExprToAddrs r s (HInvolves 1 h) =
  hExprToAddrs r s $ HMember h

hExprToAddrs r s (HInvolves k h) =
  case k < 1 of
  True -> Left $ "First argument of HInvolves must be an integer > 0."
  False -> do
    as :: Set Addr <-
      hExprToAddrs r s $ HInvolves (k-1) h
    S.union as <$>
      ( hExprToAddrs r s $ HMember $ HOr $
        map (HExpr . ExprAddr) $ S.toList as )

hExprToAddrs r s (HEval hm paths) =
  prefixLeft "hExprToAddrs, called on HEval:" $ do
    (hosts :: Set Addr) <-
      prefixLeft "computing hosts:"
      $ hExprToAddrs r s hm
    (its :: Set (Set Addr)) <-
      ifLefts_set $ S.map (subExprs r paths) hosts
    Right $ S.unions its

hExprToAddrs _ s (HVar v) =
  maybe (Left $ keyErr "hExprToAddrs" v s) (Right . S.singleton)
  $ M.lookup v s

hExprToAddrs r _ (HExpr e) = S.singleton <$> exprToAddr r e

hExprToAddrs r s (HDiff base exclude) =
  prefixLeft "hExprToAddrs, called on HDiff:" $ do
    b <- prefixLeft "calculating base:"
         $ hExprToAddrs r s base
    e <- prefixLeft "calculating exclude:"
         $ hExprToAddrs r s exclude
    Right $ S.difference b e

-- | TRICK: For speed, put the most selective searches first in the list.
hExprToAddrs r s (HAnd hs) =
  prefixLeft "hExprToAddrs, called on HAnd:"
  $ foldr1 S.intersection
  <$> ifLefts (map (hExprToAddrs r s) hs )

hExprToAddrs r s (HOr hs) =
  prefixLeft "hExprToAddrs, called on HOr:" $ do
  let found :: [Set Addr] =
        rights $ map (hExprToAddrs r s) hs
  if null found
    then Left $ "No matches."
    else Right $ S.unions found

hExprToAddrs r sub (HReach d ht hs) =
  prefixLeft "hExprToAddrs, called on HReach:" $ do
  s <- S.toList <$> hExprToAddrs r sub hs
  t <- S.toList <$> hExprToAddrs r sub ht
  S.fromList <$> reachable d r t s

hExprToAddrs r sub (HTrans d targets ht he hs) =
  -- TODO : This could be smarter. If all you want to know is
  -- which starts can reach some end,
  -- you don't need to find every such connection;
  -- you can stop looking after the first.
  -- And if you want to know which ends can be reached by some start,
  -- then once a start has reached some ends,
  -- you can remove those ends when testing the remaining starts.
  prefixLeft "hExprToAddrs, called on HTrans:" $ do
  t :: [Addr] <- S.toList <$> hExprToAddrs r sub ht
  e :: [Addr] <- S.toList <$> hExprToAddrs r sub he
  s :: [Addr] <- S.toList <$> hExprToAddrs r sub hs
  pairs :: [(Addr,Addr)] <- transitiveRels d r t e s
  let firsts = if not $ elem SearchLeftward targets then []
        else map fst pairs
      seconds = if not $ elem SearchRightward targets then []
        else map snd pairs
  Right $ S.fromList $ firsts ++ seconds

hExprToAddrs r _ HTplts =
  Right $ _tplts r


-- | PITFALL: Assumes (at least one) relationship it is given is reflexive.
transitiveClosure :: SearchDir
  -> Rslt
  -> [Addr] -- ^ binary `Tplt`s to search along.
            -- To use more than one is weird but legal.
  -> [Addr] -- ^ A subset of the graph.
  -> Either String [(Addr,Addr)]
transitiveClosure d r ts as =
  transitiveRels d r ts as as

-- | = Searching from a fixed set toward a fixed target.
-- For instance, given sets S and T, find the set {(s,t) | s in S, t in T,
-- and there exists a chain s < n1 < n2 < n3 < ... < t of length 2 or more}.

transitiveRels :: SearchDir
  -> Rslt
  -> [Addr] -- ^ binary `Tplt`s to search along.
            -- To use more than one is weird but legal.
  -> [Addr] -- ^ places to maybe finish
  -> [Addr] -- ^ places to start
  -> Either String [(Addr,Addr)]
transitiveRels d r ts fs ss =
  prefixLeft "transitiveRels:" $
    concat <$> ifLefts (map f ss)
  where f = transitiveRels1 d r ts fs

transitiveRels1 :: SearchDir
  -> Rslt
  -> [Addr] -- ^ binary `Tplt`s to search along.
            -- To use more than one is weird but legal.
  -> [Addr] -- ^ places to maybe finish
  -> Addr -- ^ the place to start
  -> Either String [(Addr,Addr)]
transitiveRels1 d r ts fs s =
  prefixLeft "transitiveRels1:" $ do
  found <- L.intersect fs <$> reachable d r ts [s]
  let pair = case d of SearchRightward -> (s,)
                       SearchLeftward  -> (,s)
  Right $ map pair found

-- | `cyclesInvolving r t a0` searches for cycles involving `a0`.
-- PITFALL: It assumes there are no other cycles.
-- If it encounters any not involving `a0`, it will crash.
cyclesInvolving :: Rslt -> SearchDir -> TpltAddr -> Addr
                -> Either String [Cycle]
cyclesInvolving r d t a0 =
  map ((t,) . reverse) <$> go [] [[a0]] where

  go :: [[Addr]] -> [[Addr]] -> Either String [[Addr]]
  go cycles []             = Right cycles
  go cycles (as:notCycles) = do
    (cs,ncs) <- extendPath as
    go (cs ++ cycles) (ncs ++ notCycles)

  extendPath :: [Addr] -> Either String
                        ( [[Addr]]   -- cycles
                        , [[Addr]] ) -- not cycles
  extendPath [] = error "impossible: paths start nonempty and only grow."
  extendPath as@(a:_) = do
    ns :: Set Addr <- immediateNeighbors r d [t] [a]
    let cycles = case S.member a0 ns of
          True -> [a0 : as]
          False -> []
        notCycles = map (: as) $ S.toList $ S.delete a0 ns
    Right (cycles, notCycles)


-- | = Searching from a fixed set of `Expr`s toward no particular target.
-- For instance, given set S, find the set T = {t s.t. t > s for some s in S}.

-- | `reachable d r ts as` finds all the expressions reachable from `as`,
-- via `ts`, by moving `d`.
--
-- PITFALL: If there are cycles, this will crash.
reachable :: SearchDir
          -> Rslt
          -> [TpltAddr] -- ^ binary `Tplt`s to search along.
                        -- To use more than one is weird but legal.
          -> [Addr] -- ^ starting `Expr`s
          -> Either String [Addr]
reachable d r ts as = prefixLeft "reachable:" $ do
  _ <- ifLefts $ map (isBinaryTemplate r) ts
  f [] as
  where
    f :: [Addr] -> [Addr] -> Either String [Addr]
    f explored [] = Right explored
    f explored (a:morePending) = do
      s <- immediateNeighbors r d ts [a]
      f (a:explored) $ morePending ++ S.toList s
      -- I believe this gives BFS,
      -- and flipping the ++ would change it to DFS.


-- | = Utilities used by the above transitive search utilities

isBinaryTemplate :: Rslt -> Addr -> Either String ()
isBinaryTemplate r t = do
  v <- variety r t
  if v == (TpltCtr,2) then Right () else Left $
    "Expr at address " ++ show t ++ " not a binary template."

immediateNeighbors :: Rslt
                   -> SearchDir
                   -> [TpltAddr] -- ^ binary `Tplt`s to search along.
                                 -- To use more than one is weird but legal.
                   -> [Addr] -- ^ starting `Expr`s
                   -> Either String (Set Addr)
immediateNeighbors r d ts as =
  let (start, toward) = case d of
        SearchRightward -> (1,2)
        SearchLeftward -> (2,1)
  in hExprToAddrs r mempty $
     HEval ( HMap $ M.fromList
             [ ( RoleInRel' $ RoleMember start
               , HOr $ map (HExpr . ExprAddr) as )
             , ( RoleInRel' RoleTplt
               , HOr $ map (HExpr . ExprAddr) ts ) ] )
     [[ RoleMember toward ]]
