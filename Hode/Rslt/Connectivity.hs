{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Connectivity where

import qualified Data.Map as M
import qualified Data.Set as S

import Hode.Hash.HLookup
import Hode.Hash.HTypes
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | `rightReachable r s t` finds all the expressions reachable from `s`,
-- by moving rightward. `s` starts as member 1 and we look for member2,
-- then each of those becomes member 1 and we look for new member 2, etc.)
rightReachable :: Rslt
               -> Addr -- ^ a binary `Tplt`
               -> Addr -- ^ a starting `Expr`
               -> Either String [Addr]

rightReachable r t s0 = prefixLeft "rightReachable: " $ do
  v <- variety r t
  if v == (TpltCtr,2) then Right () else Left $
    "Expr at address " ++ show t ++ " not a binary template."
  let goRight :: Addr -> HExpr
      goRight a = HEval
                  ( HMap $ M.fromList
                    [ (RoleMember 1, HExpr $ Addr a)
                    , (RoleTplt, HExpr $ Addr t) ] )
                  [[ RoleMember 2 ]]

      f :: [Addr] -> [Addr] -> Either String [Addr]
      f explored [] = Right explored
      f explored (a:morePending) =
        prefixLeft ("f of " ++ show a) $ do
        s <- hExprToAddrs r mempty $ goRight a
        f (a:explored) $ S.toList s ++ morePending
  f [] [s0]
