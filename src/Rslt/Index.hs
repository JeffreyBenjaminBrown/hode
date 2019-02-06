-- | Minus mkRslt, these gory details are not
-- part of the Rslt interface.

{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.Index where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.RTypes
import Rslt.RUtil


mkRslt :: RefExprs -> Rslt
mkRslt es = let
  (hasMap :: Map Addr (Map Role Addr)) =
    M.filter (not . M.null)
    $ M.map (M.fromList . refExprPositions)
    $ es
  in Rslt {
    _refExprAt = es
  , _addrOf = imgDb es
  , _variety = M.map refExprVariety es
  , _has = hasMap
  , _isIn = foldl invertAndAddPositions M.empty
            $ M.toList $ M.map M.toList hasMap
  }


-- | == Given an expression, look up an address.

imgDb :: RefExprs -> Map RefExpr Addr
imgDb = M.fromList . catMaybes . map f . M.toList where
  f (addr, expr) = case expr of
    Par' _ _ -> Nothing
    _        -> Just (expr, addr)


-- | == Given an address, look up what it's connected to.
-- The following two functions are in a sense inverses.

-- | `refExprPositions e` gives every pair `(r,a)` such that a plays the role
-- r in e.

refExprPositions :: RefExpr -> [(Role,Addr)]
refExprPositions expr =
  let r :: (Int, Addr) -> (Role, Addr)
      r (n,a) = (RoleMember n, a)
  in case expr of
    Word' _      -> []
    Tplt' mas    ->                 map r (zip [1..]           mas)
    Rel'  mas ta -> (RoleTplt,ta) : map r (zip [1..]           mas)
    Par'  sas _  ->                 map r (zip [1..] $ map snd sas)


-- | `invertAndAddPositions m (a, ras)` is meant for the case where m is a map
-- from addresses to the set of roles they play in other expressions, ras is
-- the set of roles in a, and a is not a key of m. 

invertAndAddPositions :: Map Addr (Set (Role, Addr))
                      -> (Addr,       [(Role, Addr)])
                      -> Map Addr (Set (Role, Addr))
invertAndAddPositions fm (a1, ras) = foldl f fm ras where
  f :: Map Addr (Set (Role, Addr))
    ->               (Role, Addr)
    -> Map Addr (Set (Role, Addr))
  f fm (r,a) = M.insertWith S.union a newData fm
    where newData :: Set (Role, Addr)
          newData = S.singleton (r,a1)
