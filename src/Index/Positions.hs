{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

module Index.Positions where

import FiniteMap
import SetRBT

import Rslt


-- | = `positionsWithinAll` and `positionsHeldByAll` are roughly inverses:
-- `listToFM positionsWithinAll` is a map from `Addr`es to
-- the positions  *contained by*  the `Expr` at the key `Addr`.
-- `positionsHeldByAll`          is a map from `Addr`es to
-- the positions  *that contain*  the `Expr` at the key `Addr`.

positionsWithinAll :: Files -> [(Addr, [(Role, Addr)])]
positionsWithinAll = filter (not . null . snd) . map f . fmToList where
  f :: (Addr, Expr) -> (Addr, [(Role,Addr)])
  f (a, expr) = (a, exprPositions expr)

  exprPositions :: Expr -> [(Role,Addr)]
  exprPositions expr =
    let r :: (Int, Addr) -> (Role, Addr)
        r (n,a) = (RoleMember n, a)
    in case expr of
      Word _          -> []
      Tplt mas    ->                     map r (zip [1..]           mas)
      Rel mas ta      -> (RoleTplt,ta) : map r (zip [1..]           mas)
      Par sas _ ->                     map r (zip [1..] $ map snd sas)

positionsHeldByAll :: [( Addr,       [(Role, Addr)] )]
                -> FM Addr (SetRBT (Role, Addr))
positionsHeldByAll aras = foldl addInvertedPosition (emptyFM (<)) aras where

  addInvertedPosition :: FM Addr (SetRBT (Role, Addr))
                      -> (Addr, [(Role, Addr)])
                      -> FM Addr (SetRBT (Role, Addr))
  addInvertedPosition fm (a1, ras) = foldl f fm ras where
    f :: FM Addr (SetRBT (Role, Addr))
      ->                    (Role, Addr)
      -> FM Addr (SetRBT (Role, Addr))
    f fm (r,a) = addToFM_C unionRBT fm a newData
      where newData :: SetRBT (Role, Addr)
            newData = insertRBT (r,a1) $ emptySetRBT (<)
