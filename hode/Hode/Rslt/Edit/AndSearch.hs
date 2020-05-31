-- PITFALL: These functions are all mutually recursive.
-- They mix editing and search.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Edit.AndSearch (
    exprToAddrInsert -- ^ Rslt -> Expr -> Either String
                     -- ( Rslt
                     -- , [Aged Addr] -- ^ added or already present
                     -- , [Cycle] ) -- ^ any new cycles
  , exprToAddrInsert_list -- ^ Rslt -> [Expr] -> Either String
                     -- ( Rslt
                     -- , [[Aged Addr]] -- ^ added or already present
                     -- , [Cycle] ) -- ^ any new cycles
  ) where

import qualified Data.List      as L

import Hode.Hash.Lookup
import Hode.Rslt.Binary
import Hode.Rslt.Edit.Initial
import Hode.Rslt.Lookup
import Hode.Rslt.Types
import Hode.Rslt.Util
import Hode.Util.Misc


-- | = Edit + search

-- | `exprToAddrInsert r ei` returns an `Old` containing `ei`'s `Addr,
-- if `ei` is already in `r`.
-- If not, it inserts `ei`, and then returns a `New` containing its `Addr`.
-- It also returns (the tail of the list) all other `Addr`s it added.
-- Since it might add to the `Rslt`, it also returns that.
--
-- NOTE: In any `[Aged Addr]`, the top expression's address
-- precedes those of its children.
exprToAddrInsert :: Rslt -> Expr
  -> Either String ( Rslt
                   , [Aged Addr] -- ^ added or already present
                   , [Cycle] ) -- ^ any new cycles
exprToAddrInsert r ei =
  prefixLeft ("exprToAddrInsert, called on " ++ show ei ++ ": ") $
  let mra :: Maybe Addr = either (const Nothing) Just
                          $ exprToAddr r ei
  in case mra of
    Just a -> Right (r, [Old a], [])
    Nothing -> prefixLeft "exprToAddrInsert_rootNotFound:" $
               exprToAddrInsert_rootNotFound r ei


-- | `exprToAddrInsert_rootNotFound` is like `exprToAddrInsert`, in the case
-- that the root `RefExpr` has been determined not to be present,
-- but the others still might be.
exprToAddrInsert_rootNotFound ::
  Rslt -> Expr -> Either String (Rslt, [Aged Addr], [Cycle])
exprToAddrInsert_rootNotFound _ (ExprAddr a) =
  Left $ "exprToAddrInsert: Addr " ++ show a ++ "not found."

exprToAddrInsert_rootNotFound r0 (Phrase w) = do
  a <- nextAddr r0
  r1 <- insertAt a (Phrase' w) r0
  Right (r1, [New a], [])

exprToAddrInsert_rootNotFound r0 (ExprTplt (Tplt a bs c)) = do
  if null a && null bs && null c
    then  Left "invalid Tplt: must have at least one separator."
    else Right ()
  (r1 :: Rslt, as1 :: [Aged Addr], cs1 :: [Cycle]) <- case a of
    Nothing -> Right (r0,[],[])
    Just a' -> exprToAddrInsert r0 a'
  (r2 :: Rslt
    , as2 :: [[Aged Addr]] -- note: list of lists
    , cs2 :: [Cycle]) <-
    exprToAddrInsert_list r1 bs
  (r3 :: Rslt, as3 :: [Aged Addr], cs3 :: [Cycle]) <-
    case c of
    Nothing -> Right (r2,[],[])
    Just c' -> exprToAddrInsert r2 c'
  a' <- nextAddr r3
  r4 :: Rslt <-
    let tplt :: RefExpr = Tplt' $ Tplt
          (maybe Nothing (const $ Just $ head $ map unAged as1) a)
          (map (unAged . head) as2)
          (maybe Nothing (const $ Just $ head $ map unAged as3) c)
    in insertAt a' tplt r3
  Right ( r4,
          New a' : concat ( [as1] ++ as2 ++ [as3] ),
          cs1 ++ cs2 ++ cs3 )

exprToAddrInsert_rootNotFound r0 (ExprRel (Rel ms t)) = do
  -- insert the Rel's template
  (r1,tas,tcs)  <- exprToAddrInsert r0 t
  ta <- if length tas > 0 then Right $ unAged $ head tas else Left
    "There should be an address for the Tplt. (Not a user error.)"

  -- insert the Rel's members
  (r2 :: Rslt, mas :: [[Aged Addr]], mcs :: [Cycle]) <-
    exprToAddrInsert_list r1 ms

  -- insert the Rel itself
  a  :: Addr <- nextAddr r2
  r3 :: Rslt <- let rel = Rel' $ Rel (map (unAged . head) mas) ta
                in insertAt a rel r2

  -- search for cycles
  transitive :: Bool <- usesTransitiveTplt r3 a
  cs :: [Cycle] <- let ma0 = unAged $ head $ head mas
                         -- if binary, this is the left member
    in if not transitive then Right []
       else cyclesInvolving r3 SearchLeftward ta ma0

  Right (r3, New a : tas ++ concat mas, cs ++ tcs ++ mcs)

-- | `exprToAddrInsert_list r0 is` will insert all of the `is` into `r0`.
-- If it works, it will return the new `Rslt`,
-- and an `[[Aged Addr]]` where list corresponds to an `Expr` in `is`.
-- The first `Addr` in each list is the `Addr` of the `Expr` in `is`,
-- and the rest are those of its sub-`Expr`s.
--
-- PITFALL: The top list in the `[[Aged Addr]]` corresponds to the `[Expr]`:
-- each `Expr` generates exactly one `[Aged Addr]`. By contrast,
-- the `Cycle`s are all mixed up;
-- there's no way to tell which `Cycle` comes from which `Expr`.
exprToAddrInsert_list ::
  Rslt -> [Expr] -> Either String (Rslt, [[Aged Addr]], [Cycle])
exprToAddrInsert_list r0 is =
  prefixLeft "exprToAddrInsert_list:" $ do
  let f :: Either String Rslt -> Expr
        -> ( Either String Rslt -- PITFALL: Only `Rslt` is in `Either`
           , ( [Aged Addr]
             , [Cycle] ) )
      f (Left s) _ = (Left s, error "irrelevant")
      f (Right r) ei = case exprToAddrInsert r ei of
        Left s -> (Left s, error "irrelevant")
        Right (r', as, cs) -> (Right r', (as, cs))
      ( er :: Either String Rslt,
        ascs :: [ ( [Aged Addr]
                  , [Cycle] ) ] ) =
        L.mapAccumL f (Right r0) is
  r1 <- er
  let (as :: [[Aged Addr]], cs :: [[Cycle]]) =
        unzip ascs
  Right $ (r1, as, concat cs)
