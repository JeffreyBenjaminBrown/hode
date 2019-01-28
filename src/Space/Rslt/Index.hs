module Space.Rslt.Index where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt
import Space.Rslt.RTypes
import Space.Rslt.Index.Positions
import Space.Rslt.Index.ImgLookup


-- | == Check the database

collectionsWithAbsentAddrs :: Exprs -> Rslt -> Map Addr [Addr]
collectionsWithAbsentAddrs exprs r = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . flip M.lookup (xVarieties r)

  involved :: Expr -> [Addr]
  involved (Word _)    = error "impossible"
  involved (Tplt as)   = as
  involved (Rel as a)  = a : as
  involved (Par sas _) = map snd sas

  collections :: Exprs
  collections = M.filter isCollection exprs where
    isCollection expr = case expr of Word _ -> False
                                     _      -> True

relsWithoutMatchingTplts :: Exprs -> Rslt -> Exprs
relsWithoutMatchingTplts exprs r = res where
  res = M.filter (not . relMatchesTpltArity) rels

  relMatchesTpltArity :: Expr -> Bool
  relMatchesTpltArity e@(Rel _ t) = case M.lookup t $ xVarieties r of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      Tplt' -> arity e == art
      _         -> False
  relMatchesTpltArity _ = error "relMatchesTpltArity: impossible."

  rels = M.filter isRel exprs where
    isRel (Rel _ _) = True
    isRel _         = False
