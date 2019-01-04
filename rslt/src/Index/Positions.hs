module Index.Positions where

import qualified Data.Map as M
import qualified Data.Set as S

import Rslt


-- | = `positionsWithinAll` and `positionsHeldByAll` are roughly inverses:
-- `listToFM positionsWithinAll` is a map from `Addr`es to
-- the positions  *contained by*  the `Expr` at the key `Addr`.
-- `positionsHeldByAll`          is a map from `Addr`es to
-- the positions  *that contain*  the `Expr` at the key `Addr`.

positionsWithinAll :: Files -> [(Addr, [(Role, Addr)])]
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

positionsHeldByAll :: [( Addr,         [( Role, Addr)] )]
                   -> M.Map Addr (S.Set ( Role, Addr))
positionsHeldByAll aras = foldl addInvertedPosition M.empty aras where

  addInvertedPosition :: M.Map Addr (S.Set (Role, Addr))
                      -> (Addr,           [(Role, Addr)])
                      -> M.Map Addr (S.Set (Role, Addr))
  addInvertedPosition fm (a1, ras) = foldl f fm ras where
    f :: M.Map Addr (S.Set (Role, Addr))
      ->                   (Role, Addr)
      -> M.Map Addr (S.Set (Role, Addr))
    f fm (r,a) = M.insertWith S.union a newData fm
      where newData :: S.Set (Role, Addr)
            newData = S.singleton (r,a1)
