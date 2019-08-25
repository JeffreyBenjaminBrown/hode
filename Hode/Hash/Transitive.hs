{-# LANGUAGE ScopedTypeVariables,
TupleSections #-}

module Hode.Hash.Transitive (
  transitiveRels, -- ^ :: SearchDir
                    -- -> Rslt
                    -- -> Addr -- ^ a binary `Tplt`
                    -- -> [Addr] -- ^ places to maybe finish
                    -- -> [Addr] -- ^ places to start
                    -- -> Either String [(Addr,Addr)]
  reachable -- ^ :: SearchDir
              -- -> Rslt
              -- -> Addr -- ^ a binary `Tplt`
              -- -> Addr -- ^ a starting `Expr`
              -- -> Either String [Addr]
  ) where

import qualified Data.List as L
--import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


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
transitiveRels d r ts es ss =
  concat <$>
  ifLefts (map (transitiveRels1 d r ts es) ss)

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
  _ <- ifLefts $ map (verifyBinaryTemplate r) ts
  f [] as
  where
    f :: [Addr] -> [Addr] -> Either String [Addr]
    f explored [] = Right explored
    f explored (a:morePending) =
      prefixLeft ("f of " ++ show a) $ do
        s <- immediateNeighbors d r ts [a]
        f (a:explored) $ S.toList s ++ morePending
        -- I believe this gives DFS,
        -- and flipping the ++ would change it to BFS.


-- | = Utilities

verifyBinaryTemplate :: Rslt -> Addr -> Either String ()
verifyBinaryTemplate r t = do
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
