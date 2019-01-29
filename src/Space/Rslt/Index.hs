-- | This is some gory detail, not intended to be part of the Rslt interface.

module Space.Rslt.Index where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.RTypes


-- | == Given an expression, look up an address.

imgDb :: Exprs -> Map Expr Addr
imgDb = M.fromList . catMaybes . map f . M.toList where
  f (addr, expr) = case expr of
    Par _ _ -> Nothing
    _       -> Just (expr, addr)


-- | == Given an address, look up what it's connected to.

-- | = `positionsWithinAll` and `positionsHeldByAll` are roughly inverses:
-- `listToFM positionsWithinAll` is a map from `Addr`es to
-- the positions  *contained by*  the `Expr` at the key `Addr`.
-- `positionsHeldByAll`          is a map from `Addr`es to
-- the positions  *that contain*  the `Expr` at the key `Addr`.

positionsWithinAll :: Exprs -> [(Addr, [(Role, Addr)])]
positionsWithinAll = filter (not . null . snd) . map f . M.toList where
  f :: (Addr, Expr) -> (Addr, [(Role,Addr)])
  f (a, expr) = (a, exprPositions expr)

exprPositions :: Expr -> [(Role,Addr)]
exprPositions expr =
  let r :: (Int, Addr) -> (Role, Addr)
      r (n,a) = (RoleMember n, a)
  in case expr of
    Word _     -> []
    Tplt mas   ->                 map r (zip [1..]           mas)
    Rel mas ta -> (RoleTplt,ta) : map r (zip [1..]           mas)
    Par sas _  ->                 map r (zip [1..] $ map snd sas)

positionsHeldByAll :: [( Addr,         [(Role, Addr)] )]
                   -> Map Addr (Set (Role, Addr))
positionsHeldByAll aras = foldl addInvertedPosition M.empty aras

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


-- | == Check the database

collectionsWithAbsentAddrs :: Exprs -> Rslt -> Map Addr [Addr]
collectionsWithAbsentAddrs exprs r = res where
  res = M.filter (not . null)
        $ M.map (filter absent . involved) collections

  absent :: Addr -> Bool
  absent = isNothing . flip M.lookup (_varieties r)

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
  relMatchesTpltArity e@(Rel _ t) = case M.lookup t $ _varieties r of
    Nothing         -> False
    Just (ctr, art) -> case ctr of
      Tplt' -> arity e == art
      _         -> False
  relMatchesTpltArity _ = error "relMatchesTpltArity: impossible."

  rels = M.filter isRel exprs where
    isRel (Rel _ _) = True
    isRel _         = False
