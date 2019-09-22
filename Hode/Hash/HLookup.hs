{-# LANGUAGE ScopedTypeVariables,
TupleSections #-}

module Hode.Hash.HLookup (
    hFind        -- ^ HExpr -> Find Addr Rslt
  , hMatches     -- ^ Rslt -> HExpr -> Addr       -> Either String Bool
  , hExprToExpr  -- ^ Rslt -> HExpr               -> Either String Expr
  , hExprToAddrs -- ^ Rslt -> Subst Addr -> HExpr -> Either String (Set Addr)

  -- | == Transitive search utilities
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
  , reachable -- ^ :: SearchDir
                -- -> Rslt
                -- -> [Addr] -- ^ binary `Tplt`s to search along.
                -- To use more than one is weird but legal.
                -- -> [Addr] -- ^ places to start
                -- -> Either String [Addr]
  ) where

import qualified Data.List      as L
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Hash.HTypes
import Hode.Hash.HUtil
import Hode.Qseq.QTypes
import Hode.Rslt.RTypes
import Hode.Rslt.RLookup
import Hode.Util.Misc


hFind :: HExpr -> Find Addr Rslt
hFind he = Find f $ hVars he
  where f rslt subst = hExprToAddrs rslt subst he

-- | The idea of `hMatches` is to determine whether an `HExpr`
-- matches the `Expr` at an `Addr`, without having to find everything
-- else that matches the `HExpr`. It isn't totally implemented.
hMatches :: Rslt -> HExpr -> Addr -> Either String Bool
hMatches r h0 a0 = prefixLeft "hMatches: " $ do
  e0 :: Expr <- addrToExpr r a0
  case h0 of
    HExpr e -> unAddr r e >>= Right . (== e0)

    HMap (mh :: Map Role HExpr) -> do
      ma :: Map Role Addr <- ifLefts_map $
        M.mapWithKey (\rol _ -> hasInRole r rol a0) mh
      let mPairs :: Map Role (HExpr,Addr) =
            M.mapWithKey f mh where
            f rol h = (h,a) where
              a = maybe (error "impossible") id $
                  M.lookup rol ma
      mCompares :: Map Role Bool <- ifLefts_map $
        M.map (uncurry $ hMatches r) mPairs
      Right $ and mCompares

    (HEval _ _) -> do -- TODO ? speed: Arguably, this branch cheats.
    -- It finds everything that `h0` matches, and then sees if  `a0`
    -- is in that set. But to search up from a0 instead of down from h0
    --  would still be a tree search, so it's not clear which is better.
    -- (If a0 is a member of many things, better to search from h0.)
      hImages :: Set Addr <- hExprToAddrs r mempty h0
      Right $ S.member a0 hImages

    HVar _ -> Left "HVar: not implemented."

    HDiff beIn beNotIn -> do
      bi :: Bool <- hMatches r beIn a0
      bni :: Bool <- hMatches r beNotIn a0
      Right $ bi && not bni

    HAnd hs -> do
      bs :: [Bool] <- ifLefts $ map (\h -> hMatches r h a0) hs
      Right $ and bs

    HOr hs -> do
      bs :: [Bool] <- ifLefts $ map (\h -> hMatches r h a0) hs
      Right $ or bs

    HReach _ _ _ -> error "not implemented: Hash.HLookup.hMatches, called on an HReach"
    HTrans _ _ _ _ _ -> error "not implemented: Hash.HLookup.hMatches, called on an HTrans"

-- | `hExprToExpr` is useful for parsing a not-yet-extant `Expr`.
hExprToExpr :: Rslt -> HExpr -> Either String Expr
hExprToExpr _ (HExpr e) = Right e

hExprToExpr r h@(HMap mh) =
  prefixLeft "hExprToExpr, called on HMap: " $ do
    (me :: Map Role Expr) <- ifLefts_map
                             $ M.map (hExprToExpr r) mh
    (t :: Expr) <-
      maybe (Left $ "No Tplt in " ++ show h)
      Right $ M.lookup RoleTplt me
    case t of ExprTplt _ -> Right ()
              x -> Left $ "hExprToExpr: in " ++ show h
                   ++ ", the expression " ++ show x ++ " is not a Tplt."
    ta <- arity r t
    if M.size (M.delete RoleTplt me) == ta then Right ()
      else Left $ "hExprToExpr: arity mismatch between "
           ++ show h ++ " and its Tplt " ++ show t
    Right $ ExprRel $ Rel (M.elems $ M.delete RoleTplt me) t
      -- PITFALL: This M.elems clause relies on the fact that those
      -- elems will be ordered from RoleMember 1 to RoleMember n.
      -- That's true for the same reason this is true:
      --     > M.elems $ M.fromList [(1,"z"),(2,"a")]
      --     ["z","a"]

hExprToExpr _ h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr and HMap constructors can be so converted."


-- | `hExprToAddrs` is the Hash search workhorse.

hExprToAddrs :: Rslt -> Subst Addr -> HExpr -> Either String (Set Addr)

hExprToAddrs r s (HMap m) =
  prefixLeft "-> hExprToAddrs called on HMap" $ do
    (found :: Map Role (Set Addr)) <-
      prefixLeft ", calculating found"
      $ ifLefts_map $ M.map (hExprToAddrs r s) m

    let roleHostCandidates :: Role -> Set Addr -> Either String (Set Addr)
        roleHostCandidates role as = do
          -- The `as` are presumed to fill the role `role` in some host.
          -- This returns all those hosts.
          (roleHostPairs :: Set (Role, Addr)) <-
            prefixLeft ",on HMap / f" $ S.unions
             <$> ifLefts_set (S.map (isIn r) as )
          Right $ S.map snd
            $ S.filter ((==) role . fst) roleHostPairs

    (hosts :: Map Role (Set Addr)) <-
      prefixLeft ", calculating hosts"
      $ ifLefts_map $ M.mapWithKey roleHostCandidates found
    case null hosts of
      True -> Right S.empty
      False -> Right $ foldl1 S.intersection $ M.elems hosts

hExprToAddrs r s (HEval hm paths) =
  prefixLeft "-> hExprToAddrs called on HEval" $ do
    (hosts :: Set Addr)     <-
      prefixLeft ", mapping over hosts"
      $ hExprToAddrs r s hm
    (its :: Set (Set Addr)) <-
      ifLefts_set $ S.map (subExprs r paths) hosts
    Right $ S.unions its

hExprToAddrs _ s (HVar v) =
  maybe (Left $ keyErr "hExprToAddrs" v s) (Right . S.singleton)
  $ M.lookup v s

hExprToAddrs r _ (HExpr e) = S.singleton <$> exprToAddr r e

hExprToAddrs r s (HDiff base exclude) =
  prefixLeft "-> hExprToAddrs called on HDiff" $ do
    b <- prefixLeft ", calculating base"
         $ hExprToAddrs r s base
    e <- prefixLeft ", calculating exclude"
         $ hExprToAddrs r s exclude
    Right $ S.difference b e

-- | TRICK: For speed, put the most selective searches first in the list.
hExprToAddrs r s (HAnd hs) =
  prefixLeft "-> hExprToAddrs called on HAnd"
  $ foldr1 S.intersection
  <$> ifLefts (map (hExprToAddrs r s) hs )

hExprToAddrs r s (HOr hs) =
  prefixLeft "-> hExprToAddrs called on HOr"
  $ foldr1 S.union
  <$> ifLefts (map (hExprToAddrs r s) hs )

hExprToAddrs r sub (HReach d ht hs) =
  prefixLeft "-> hExprToAddrs called on HReach" $ do
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
  prefixLeft "-> hExprToAddrs called on HTrans" $ do
  t :: [Addr] <- S.toList <$> hExprToAddrs r sub ht
  e :: [Addr] <- S.toList <$> hExprToAddrs r sub he
  s :: [Addr] <- S.toList <$> hExprToAddrs r sub hs
  pairs :: [(Addr,Addr)] <- transitiveRels d r t e s
  let firsts = if not $ elem SearchLeftward targets then []
        else map fst pairs
      seconds = if not $ elem SearchRightward targets then []
        else map snd pairs
  Right $ S.fromList $ firsts ++ seconds


-- | == Transitive search utilities

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
  concat <$>
  ifLefts (map (transitiveRels1 d r ts fs) ss)

transitiveRels1 :: SearchDir -- ^ whether to search rightward
  -> Rslt
  -> [Addr] -- ^ binary `Tplt`s to search along.
            -- To use more than one is weird but legal.
  -> [Addr] -- ^ places to maybe finish
  -> Addr -- ^ the place to start
  -> Either String [(Addr,Addr)]
transitiveRels1 d r ts fs s =
  prefixLeft "transitiveRels: " $ do
  found <- L.intersect fs <$> reachable d r ts [s]
  let pair = case d of SearchRightward -> (s,)
                       SearchLeftward  -> (,s)
  Right $ map pair found


-- | = Searching from a fixed set of `Expr`s toward no particular target.
-- For instance, given set S, find the set T = {t s.t. t > s for some s in S}.

-- | `reachable d r s t` finds all the expressions reachable from `s`,
-- via `t`, by moving `d`.
reachable :: SearchDir
          -> Rslt
          -> [Addr] -- ^ binary `Tplt`s to search along.
                    -- To use more than one is weird but legal.
          -> [Addr] -- ^ starting `Expr`s
          -> Either String [Addr]
reachable d r ts as = prefixLeft "reachable: " $ do
  _ <- ifLefts $ map (isBinaryTemplate r) ts
  f [] as
  where
    f :: [Addr] -> [Addr] -> Either String [Addr]
    f explored [] = Right explored
    f explored (a:morePending) =
      prefixLeft ("f of " ++ show a) $ do
        s <- immediateNeighbors d r ts [a]
        f (a:explored) $ morePending ++ S.toList s
        -- I believe this gives BFS,
        -- and flipping the ++ would change it to DFS.


-- | = Utilities used bye the above transitive search utilities

isBinaryTemplate :: Rslt -> Addr -> Either String ()
isBinaryTemplate r t = do
  v <- variety r t
  if v == (TpltCtr,2) then Right () else Left $
    "Expr at address " ++ show t ++ " not a binary template."

immediateNeighbors :: SearchDir
                   -> Rslt
                   -> [Addr] -- ^ binary `Tplt`s to search along.
                             -- To use more than one is weird but legal.
                   -> [Addr] -- ^ starting `Expr`s
                   -> Either String (Set Addr)
immediateNeighbors d r ts as =
  let (start, toward) = case d of
        SearchRightward -> (1,2)
        SearchLeftward -> (2,1)
  in hExprToAddrs r mempty $
     HEval ( HMap $ M.fromList
             [ ( RoleMember start
               , HOr $ map (HExpr . Addr) as )
             , ( RoleTplt
               , HOr $ map (HExpr . Addr) ts ) ] )
     [[ RoleMember toward ]]
