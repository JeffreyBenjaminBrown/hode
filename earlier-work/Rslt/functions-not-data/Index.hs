module Data.Rslt.Index where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Data.Rslt.RTypes
import Data.Rslt.Index.Positions
import Data.Rslt.Index.ImgLookup


-- | == Build the database

-- TODO (#strict) Evaluate `Index` completely at start of program.
mkIndex :: Exprs -> Index
mkIndex exprs = Index { _addrOf          = imgLookup exprs
                      , _variety         = _variety'
                      , _positionsIn     = _positionsIn'
                      , _positionsHeldBy = _positionsHeldBy'
                      }
 where
  fps = positionsWithinAll exprs :: [(Addr, [(Role, Addr)])]

  _variety' :: Addr -> Maybe (ExprCtr, Arity)
  _variety' = flip M.lookup variety where
    -- (#strict) Build `variety` completely first.
    variety = M.map exprVariety exprs

  _positionsIn' :: Addr -> Maybe (Map Role Addr)
  _positionsIn' = flip M.lookup positions where
    -- (#strict) Build `positions` completely first.
    positions :: Map Addr (Map Role Addr)
    positions = M.map M.fromList $ M.fromList fps

  _positionsHeldBy' :: Addr -> Maybe (Set (Role, Addr))
  _positionsHeldBy' = flip M.lookup $ positionsHeldByAll fps
    -- (#strict) Build `positionsHeldByAll fps` completely first.


imgLookup :: Exprs -> (ImgOfExpr -> Maybe Addr)
imgLookup exprs img = let idb = imgDb exprs in case img of

  ImgOfExpr e -> M.lookup e idb
  ImgOfAddr a -> maybe Nothing (const $ Just a) $ M.lookup a exprs

  ImgOfTplt is -> do
    mas <- ifNothings $ map (imgLookup exprs) is
    M.lookup (Tplt mas) idb

  ImgOfRel is i -> do
    mas <- ifNothings $ map (imgLookup exprs) is
    ma <- imgLookup exprs i
    M.lookup (Rel mas ma) idb
