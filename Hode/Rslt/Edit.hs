{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Hode.Rslt.Edit (
    exprToAddrInsert      -- ^ Rslt -> Expr   -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr] -> Either String (Rslt, [Addr])
  , delete         -- ^ Addr ->                 Rslt -> Either String Rslt
  , replaceExpr    -- ^ Expr -> Addr ->         Rslt -> Either String (Rslt, Addr)
  , replaceRefExpr -- ^ RefExpr -> Addr ->      Rslt -> Either String Rslt
  , replaceInRole  -- ^ Role -> Addr -> Addr -> Rslt -> Either String Rslt
  ) where

import           Control.Lens hiding (has, re)
import           Data.Either
import           Data.Functor (void)
import qualified Data.List      as L
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Hode.Rslt.Index
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.RValid
import Hode.Util.Misc
import Hode.Rslt.Edit.Initial


-- | = Edit + search

-- | = PITFALL: mutual recursion in the next 3 functions:
-- `exprToAddrInsert`, `exprToAddrInsert_rootNotFound`,
-- and `exprToAddrInsert_list`

-- | `exprToAddrInsert r ei` returns the `Addr` containing ei, if present.
-- If not, it inserts ei, and then returns the `Addr` containing it.
-- Since it might modify the `Rslt`, it also returns that.
exprToAddrInsert :: Rslt -> Expr -> Either String (Rslt, Addr)
exprToAddrInsert r ei = do
  let (mra :: Maybe Addr) = either (const Nothing) Just
                            $ exprToAddr r ei
  case mra of
    Just a -> Right (r, a)
    Nothing -> exprToAddrInsert_rootNotFound r ei


-- | `exprToAddrInsert_rootNotFound` is like `exprToAddrInsert`, in the case
-- that the root `RefExpr` has been determined not to be present,
-- but the others still might be.
exprToAddrInsert_rootNotFound :: Rslt -> Expr -> Either String (Rslt, Addr)
exprToAddrInsert_rootNotFound _ (Addr a) =
  Left $ "exprToAddrInsert: Addr " ++ show a ++ "not found.\n"

exprToAddrInsert_rootNotFound r0 (Phrase w) = do
  a <- nextAddr r0
  r1 <- insertAt a (Phrase' w) r0
  Right (r1,a)

exprToAddrInsert_rootNotFound r0 (ExprTplt js) = do
  (r1,as) <- prefixLeft "exprToAddrInsert_rootNotFound"
            $ exprToAddrInsert_list r0 js
  a <- nextAddr r1
  r2 <- insertAt a (Tplt' $ as) r1
  Right (r2, a)

exprToAddrInsert_rootNotFound r0 (ExprRel (Rel ms t)) =
  prefixLeft "exprToAddrInsert_rootNotFound" $ do
  (r1,ta)  <- exprToAddrInsert r0 t
  (r2,mas) <-  exprToAddrInsert_list r1 ms
  a <- nextAddr r2
  r3 <- insertAt a (Rel' $ Rel mas ta) r2
  Right (r3,a)


exprToAddrInsert_list :: Rslt -> [Expr] -> Either String (Rslt, [Addr])
exprToAddrInsert_list r0 is = do
  let ((er, as) :: (Either String Rslt, [Addr])) =
        L.mapAccumL f (Right r0) is where
        f :: Either String Rslt -> Expr -> (Either String Rslt, Addr)
        f (Left s) _ = (Left s, error "irrelevant")
        f (Right r) ei = case exprToAddrInsert r ei of
                           Left s -> (Left s, error "irrelevant")
                           Right (r',a) -> (Right r', a)
  r1 <- prefixLeft "exprToAddrInsert_list" er
  Right $ (r1, as)


-- | = Pure editing

delete :: Addr -> Rslt -> Either String Rslt
delete a r = prefixLeft "delete" $ do
  hosts <- isIn r a
  if not $ null hosts
    then Left "Expr to be deleted is a member of other Exprs."
    else replaceExpr a (Phrase "") r -- TODO ? is this okay

-- | `replaceExpr a e r` replaces the `Expr` at `a`. The set of `Addr`s in
-- `r` remains unchanged.
replaceExpr :: Addr -> Expr -> Rslt -> Either String Rslt
replaceExpr a0 e0 r0 = prefixLeft "replaceExpr" $
                       go a0 anAbsentPhrase r0 >>=
                       go a0 e0
  where

    -- PITFALL: If the new `Expr` contains the old one, `go` will crash.
    -- That's why `anAbsentPhrase` is used.
    go :: Addr -> Expr -> Rslt -> Either String Rslt
    go a e r = prefixLeft "replaceExpr" $ do
      (r1 :: Rslt, a1 :: Addr) <- exprToAddrInsert r e
      rx1 :: RefExpr           <- addrToRefExpr r1 a1
      r2 :: Rslt               <- replaceRefExpr rx1 a r1
      Right $ renameAddr_unsafe a1 a r2

    anAbsentPhrase :: Expr
    anAbsentPhrase = Phrase $ aap s0 where
      s0 = "&b(;HG65Lcsd.21%^!#$o6tB6*)"
      aap :: String -> String
      aap s = if isRight $ refExprToAddr r0 (Phrase' s)
              then s else s ++ s


replaceInRole :: Role -> Addr -> Addr -> Rslt -> Either String Rslt
replaceInRole spot new host r = prefixLeft "replaceInRole" $ do
  _                          <- addrToRefExpr r new
  oldHostRefExpr             <- addrToRefExpr r host
  (hostHas :: Map Role Addr) <- has r host
  (old :: Addr) <- let err = Left $ "replaceInRole: RefExpr at " ++ show host
                             ++ " includes no position " ++ show spot ++ "\n."
    in maybe err Right $ M.lookup spot hostHas

  (newHostRefExpr :: RefExpr) <-
    _replaceInRefExpr r spot new oldHostRefExpr
  (newIsAlreadyIn :: Set (Role,Addr)) <- isIn r new

  Right $ r {
      _addrToRefExpr = M.insert host newHostRefExpr
                       $ _addrToRefExpr r
    , _refExprToAddr = M.insert newHostRefExpr host
                       $ M.delete oldHostRefExpr
                       $ _refExprToAddr r

    , _has    = M.adjust (M.insert spot new) host $ _has r

    , _isIn   =   M.filter (not . null)
      -- PITFALL: delete before inserting. Otherwise, replacing something
      -- with itself is not the identity operation.
                . M.insert new (S.insert (spot, host) newIsAlreadyIn)
      -- PITFALL: We can't adjust the value at new; it might not exist.
                . M.adjust (S.delete (spot, host)) old
                $ _isIn r
    }

-- | `replaceRefExpr re oldAddr r0` deletes the `Expr` at `oldAddr`,
-- creates or finds the `RefExpr` to replace it,
-- and substitutes the new one for the old one everywhere it appeared.

replaceRefExpr :: RefExpr -> Addr -> Rslt -> Either String Rslt
replaceRefExpr re oldAddr r0 = prefixLeft "replace" $
  case refExprToAddr r0 re of
    Right newAddr -> do
      r2 <- _substitute newAddr oldAddr r0
      deleteIfUnused oldAddr r2
    Left _ -> do
      newAddr <- nextAddr r0
      _       <- validRefExpr r0 re
      r1      <- insertAt newAddr re r0
      r2      <- _substitute newAddr oldAddr r1
      deleteIfUnused oldAddr r2

_substitute :: Addr -> Addr -> Rslt -> Either String Rslt
_substitute new old r0 = do
  (roles :: Set (Role, Addr)) <- prefixLeft "_substitute"
                                 $ isIn r0 old
  let f :: Either String Rslt -> (Role, Addr) -> Either String Rslt
      f e@(Left _) _ = e
      f (Right r) (role,host) = replaceInRole role new host r
  S.foldl f (Right r0) roles
