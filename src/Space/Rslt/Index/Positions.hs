module Space.Rslt.Index.Positions where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.RTypes


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
positionsHeldByAll aras = foldl addInvertedPosition M.empty aras where

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
