{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Edit.Terminal (
    delete      -- ^ Addr ->         Rslt -> Either String Rslt
  , replaceExpr -- ^ Expr -> Addr -> Rslt -> Either String (Rslt, Addr)
  ) where

import           Data.Either

import Hode.Rslt.Edit.AndSearch
import Hode.Rslt.Edit.Initial
import Hode.Rslt.Edit.Replace
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Util.Misc


-- | = Pure editing

delete :: Addr -> Rslt -> Either String Rslt
delete a r = prefixLeft "delete:" $ do
  hosts <- isIn r a
  if not $ null hosts
    then Left "Expr to be deleted is a member of other Exprs."
    else replaceExpr a (Phrase "") r -- TODO ? is this okay?
  -- I thought I could prevent `Phrase ""` from changing address
  -- by doing this:
  --   replaceExpr a (Phrase "") r >>=
  --   replaceExpr 0 (Phrase "")
  -- but that's impossible because after the >>=
  -- there is nothing at `Addr 0` to replace.

-- | `replaceExpr a e r` replaces the `Expr` at `a`.
-- The set of `Addr`s in `r` remains unchanged.
replaceExpr :: Addr -> Expr -> Rslt -> Either String Rslt
replaceExpr a0 e0 r0 = prefixLeft "replaceExpr:" $
                       go a0 anAbsentPhrase r0 >>=
                       go a0 e0
  where

    -- PITFALL: If the new `Expr` contains the old one, `go` will crash.
    -- That's why `anAbsentPhrase` is used.
    go :: Addr -> Expr -> Rslt -> Either String Rslt
    go a e r = prefixLeft "replaceExpr:" $ do
      (r1 :: Rslt, aes :: [Aged Addr]) <- exprToAddrInsert r e
      a1 :: Addr <- if length aes > 0 then Right $ unAged $ head aes else
        Left "There should be an address for the Tplt. (Not a user error.)"
      rx1 :: RefExpr             <- addrToRefExpr r1 a1
      r2 :: Rslt                 <- replaceRefExpr rx1 a r1
      Right $ renameAddr_unsafe a1 a r2

    anAbsentPhrase :: Expr
    anAbsentPhrase = Phrase $ aap s0 where
      s0 = "&b(;HG65Lcsd.21%^!#$o6tB6*)"
      aap :: String -> String
      aap s = if isRight $ refExprToAddr r0 (Phrase' s)
              then s else s ++ s
