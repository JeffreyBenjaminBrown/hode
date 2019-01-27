module Space.Rslt.Index where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.RTypes
import Space.Rslt.Index.Positions
import Space.Rslt.Index.ImgLookup


-- | == Build the database

-- TODO (#strict) Evaluate `Index` completely at start of program.
mkIndex :: Exprs -> Index
mkIndex files = Index { _addrOf          = imgLookup files
                      , _variety         = _variety'
                      , _positionsIn     = _positionsIn'
                      , _positionsHeldBy = _positionsHeldBy'
                      }
 where
  fps = positionsWithinAll files :: [(Addr, [(Role, Addr)])]

  _variety' :: Addr -> Maybe (ExprCtr, Arity)
  _variety' = flip M.lookup varieties where
    -- (#strict) Build `varieties` completely first.
    varieties = M.map exprVariety files

  _positionsIn' :: Addr -> Maybe (Map Role Addr)
  _positionsIn' = flip M.lookup positions where
    -- (#strict) Build `positions` completely first.
    positions :: Map Addr (Map Role Addr)
    positions = M.map M.fromList $ M.fromList fps

  _positionsHeldBy' :: Addr -> Maybe (Set (Role, Addr))
  _positionsHeldBy' = flip M.lookup $ positionsHeldByAll fps
    -- (#strict) Build `positionsHeldByAll fps` completely first.


-- | == Check the database

collectionsWithAbsentAddrs :: Exprs -> Index -> Map Addr [Addr]
collectionsWithAbsentAddrs files index = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . _variety index

  involved :: Expr -> [Addr]
  involved (Word _)    = error "impossible"
  involved (Tplt as)   = as
  involved (Rel as a)  = a : as
  involved (Par sas _) = map snd sas

  collections :: Exprs
  collections = M.filter isCollection files where
    isCollection expr = case expr of Word _ -> False
                                     _      -> True

relsWithoutMatchingTplts :: Exprs -> Index -> Exprs
relsWithoutMatchingTplts files index = res where
  res = M.filter (not . relMatchesTpltArity) rels

  relMatchesTpltArity :: Expr -> Bool
  relMatchesTpltArity e@(Rel _ t) = case _variety index t of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      Tplt' -> arity e == art
      _         -> False
  relMatchesTpltArity _ = error "relMatchesTpltArity: impossible."

  rels = M.filter isRel files where
    isRel (Rel _ _) = True
    isRel _         = False
