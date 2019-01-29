-- | Gory details, not part of the Rslt interface.

module Data.Rslt.Index where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.RTypes


-- | == Given an expression, look up an address.

imgDb :: Exprs -> Map Expr Addr
imgDb = M.fromList . catMaybes . map f . M.toList where
  f (addr, expr) = case expr of
    Par _ _ -> Nothing
    _       -> Just (expr, addr)


-- | == Given an address, look up what it's connected to.
-- The following two functions are in a sense inverses.

-- | `exprPositions e` gives every pair `(r,a)` such that a plays the role
-- r in e.

exprPositions :: Expr -> [(Role,Addr)]
exprPositions expr =
  let r :: (Int, Addr) -> (Role, Addr)
      r (n,a) = (RoleMember n, a)
  in case expr of
    Word _     -> []
    Tplt mas   ->                 map r (zip [1..]           mas)
    Rel mas ta -> (RoleTplt,ta) : map r (zip [1..]           mas)
    Par sas _  ->                 map r (zip [1..] $ map snd sas)


-- | `addInvertedPosition m (a, ras)` is meant for the case where m is a map
-- from addresses to the set of roles they play in other expressions, ras is
-- the set of roles played by a, and a is not a key of m. The function merges
-- (a, ras) into m; that is, it produces a map which incorporates the information
-- from both arguments.

addInvertedPosition :: Map Addr (Set (Role, Addr))
                    -> (Addr,       [(Role, Addr)])
                    -> Map Addr (Set (Role, Addr))
addInvertedPosition fm (a1, ras) = foldl f fm ras where
  f :: Map Addr (Set (Role, Addr))
    ->               (Role, Addr)
    -> Map Addr (Set (Role, Addr))
  f fm (r,a) = M.insertWith S.union a newData fm
    where newData :: Set (Role, Addr)
          newData = S.singleton (r,a1)

