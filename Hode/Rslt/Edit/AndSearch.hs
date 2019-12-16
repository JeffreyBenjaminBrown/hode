-- PITFALL: These functions are all mutually recursive.
-- They mix editing and search.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Edit.AndSearch (
    exprToAddrInsert      -- ^ Rslt -> Expr   -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr] -> Either String (Rslt, [Addr])
  ) where

import qualified Data.List      as L

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc
import Hode.Rslt.Edit.Initial


-- | = Edit + search

-- | `exprToAddrInsert r ei` returns the `Addr` containing `ei`, if present.
-- If not, it inserts `ei`, and then returns the `Addr` containing it.
-- It also returns (the tail of the list) all other `Addr`s it added.
-- Since it might add to the `Rslt`, it also returns that.
--
-- NOTE: In any `[Aged Addr]`, the top expression's address
-- precedes those of its children.
exprToAddrInsert :: Rslt -> Expr -> Either String (Rslt, [Aged Addr])
exprToAddrInsert r ei =
  prefixLeft ("exprToAddrInsert, called on " ++ show ei ++ ":\n")
  $ do
  let (mra :: Maybe Addr) = either (const Nothing) Just
                            $ exprToAddr r ei
  case mra of
    Just a -> Right (r, [Old a])
    Nothing -> exprToAddrInsert_rootNotFound r ei


-- | `exprToAddrInsert_rootNotFound` is like `exprToAddrInsert`, in the case
-- that the root `RefExpr` has been determined not to be present,
-- but the others still might be.
exprToAddrInsert_rootNotFound ::
  Rslt -> Expr -> Either String (Rslt, [Aged Addr])
exprToAddrInsert_rootNotFound _ (Addr a) =
  Left $ "exprToAddrInsert: Addr " ++ show a ++ "not found.\n"

exprToAddrInsert_rootNotFound r0 (Phrase w) =
  prefixLeft "exprToAddrInsert_rootNotFound:" $ do
  a <- nextAddr r0
  r1 <- insertAt a (Phrase' w) r0
  Right (r1, [New a])

exprToAddrInsert_rootNotFound r0 (ExprTplt (Tplt a bs c)) =
  prefixLeft "exprToAddrInsert_rootNotFound:" $ do
  if null a && null bs && null c
    then  Left "invalid Tplt: must have at least one separator."
    else Right ()
  (r1 :: Rslt, as1 :: [Aged Addr]) <- case a of
    Nothing -> Right (r0,[])
    Just a' -> exprToAddrInsert r0 a'
  (r2 :: Rslt, as2 :: [[Aged Addr]]) <- -- note: list of lists
    exprToAddrInsert_list r1 bs
  (r3 :: Rslt, as3 :: [Aged Addr]) <- case c of
    Nothing -> Right (r2,[])
    Just c' -> exprToAddrInsert r2 c'
  a' <- nextAddr r3
  r4 :: Rslt <-
    let tplt :: RefExpr = Tplt' $ Tplt
          (maybe Nothing (const $ Just $ head $ map unAged as1) a)
          (map (unAged . head) as2)
          (maybe Nothing (const $ Just $ head $ map unAged as3) c)
    in insertAt a' tplt r3
  Right ( r4,
          New a' : concat ( [as1] ++ as2 ++ [as3] ) )

exprToAddrInsert_rootNotFound r0 (ExprRel (Rel ms t)) =
  prefixLeft "exprToAddrInsert_rootNotFound:" $ do
  (r1,tas)  <- exprToAddrInsert r0 t
  ta <- if length tas > 0 then Right $ unAged $ head tas else Left
    "There should be an address for the Tplt. (Not a user error.)"
  (r2 :: Rslt, mas :: [[Aged Addr]]) <-  exprToAddrInsert_list r1 ms
  a <- nextAddr r2
  r3 <- let rel = Rel' $ Rel (map (unAged . head) mas) ta
        in insertAt a rel r2
  Right (r3, New a : tas ++ concat mas)

-- | `exprToAddrInsert_list r0 is` will insert all of the `is` into `r0`.
-- If it works, it will return the new `Rslt`,
-- and an `[[Aged Addr]]` where list corresponds to an `Expr` in `is`.
-- The first `Addr` in each list is the `Addr` of the `Expr` in `is`,
-- and the rest are those of its sub-`Expr`s.

exprToAddrInsert_list ::
  Rslt -> [Expr] -> Either String (Rslt, [[Aged Addr]])
exprToAddrInsert_list r0 is =
  prefixLeft "exprToAddrInsert_list:" $ do
  let f :: Either String Rslt -> Expr
        -> (Either String Rslt, [Aged Addr])
      f (Left s) _ = (Left s, error "exprToAddrInsert_list: irrelevant")
      f (Right r) ei = case exprToAddrInsert r ei of
        Left s -> (Left s, error "exprToAddrInsert_list: irrelevant")
        Right (r',as) -> (Right r', as)
      (er, asas) :: (Either String Rslt, [[Aged Addr]]) =
        L.mapAccumL f (Right r0) is
  r1 <- er
  Right $ (r1, asas)
