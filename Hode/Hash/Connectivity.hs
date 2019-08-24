{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Hash.Connectivity (
  rightReachable, leftReachable -- ^ Rslt
                  -- -> Addr -- ^ a binary `Tplt`
                  -- -> Addr -- ^ a starting `Expr`
                  -- -> Either String [Addr]
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | = Searching from a fixed set toward a fixed target.
-- For instance, given sets S and T, find the set {(s,t) | s in S, t in T,
-- and there exists a chain s < n1 < n2 < n3 < ... < t of length 2 or more}.


-- <<< TODO : RESUME HERE >>>


-- | = Searching from a fixed set of `Expr`s toward no particular target.
-- For instance, given set S, find the set T = {t s.t. t > s for some s in S}.

-- | `rightReachable r s t` finds all the expressions reachable from `s`,
-- by moving rightward. `s` starts as member 1 and we look for member2,
-- then each of those becomes member 1 and we look for new member 2, etc.)

rightReachable, leftReachable ::
  Rslt
  -> Addr -- ^ a binary `Tplt`
  -> Addr -- ^ a starting `Expr`
  -> Either String [Addr]
rightReachable = reachable True
leftReachable  = reachable False


-- | = Not for export.

reachable :: Bool -- ^ whether to search rightward
          -> Rslt
          -> Addr -- ^ a binary `Tplt`
          -> Addr -- ^ a starting `Expr`
          -> Either String [Addr]
reachable rightward r t s0 = prefixLeft "reachable: " $ do
  v <- variety r t
  if v == (TpltCtr,2) then Right () else Left $
    "Expr at address " ++ show t ++ " not a binary template."
  let f :: [Addr] -> [Addr] -> Either String [Addr]
      f explored [] = Right explored
      f explored (a:morePending) =
        prefixLeft ("f of " ++ show a) $ do
        s <- hExprToAddrs r mempty $ immediateNeighbors rightward t [a]
        f (a:explored) $ S.toList s ++ morePending
  f [] [s0]

immediateNeighbors :: Bool -- ^ whether searching rightward or leftward
                   -> Addr -- ^ a binary `Tplt`
                   -> [Addr] -- ^ some starting `Expr`s
                   -> HExpr
immediateNeighbors rightward t as =
  let (start, toward) = case rightward of
        True -> (1,2)
        False -> (2,1)
  in HEval ( HMap $ M.fromList
             [ (RoleMember start, HOr $ map (HExpr . Addr) as)
             , (RoleTplt, HExpr $ Addr t) ] )
     [[ RoleMember toward ]]
