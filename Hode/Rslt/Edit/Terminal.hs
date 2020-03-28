{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Hode.Rslt.Edit.Terminal (
    moveRefExpr -- ^ Addr -> Addr -> Rslt -> Either String Rslt
  , replaceExpr -- ^ Expr -> Addr -> Rslt -> Either String (Rslt,[Cycle])

  ) where

import           Data.Either
import qualified Data.Map as M

import Hode.Rslt.Edit.AndSearch
import Hode.Rslt.Edit.Initial
import Hode.Rslt.Edit.Replace
import Hode.Rslt.Lookup
import Hode.Rslt.Types
import Hode.Rslt.Util (nextAddr)
import Hode.Util.Misc


moveRefExpr :: Addr -> Addr -> Rslt -> Either String Rslt
-- PITFALL: lots of intentional name-shadowing
moveRefExpr old new r =
  prefixLeft "moveRefExpr:" $ let
  go :: Addr -> Addr -> Rslt -> Either String Rslt
  go old new r = do
    -- PITFALL: Assumes `new` is empty.
    re <- addrToRefExpr r old
    r <- insertAt new re r
    r <- substitute old new r
    deleteIfUnused old r

  in if null $ M.lookup new $ _addrToRefExpr r
     then go old new r
     else do na <- nextAddr r
             r <- go new na r
             go old new r

-- | `replaceExpr a e r` replaces the `Expr` at `a`.
-- The set of `Addr`s in `r` remains unchanged.
replaceExpr :: Addr -> Expr -> Rslt -> Either String (Rslt,[Cycle])
replaceExpr a0 e0 r0 = prefixLeft "replaceExpr:" $ do
  (r1,cs1) <- go a0 anAbsentPhrase r0
  (r2,cs2) <- go a0 e0 r1
  Right (r2, cs1++cs2)
  where

    -- PITFALL: If the new `Expr` contains the old one, `go` will crash.
    -- That's why `anAbsentPhrase` is used.
    go :: Addr -> Expr -> Rslt -> Either String (Rslt, [Cycle])
    go a e r = do
      (r1 :: Rslt, aes :: [Aged Addr], cs :: [Cycle]) <-
        exprToAddrInsert r e
      a1 :: Addr <- if length aes > 0
        then Right $ unAged $ head aes
        else Left "There should be an address for the Tplt. (Not a user error.)"
      rx1 :: RefExpr <- addrToRefExpr r1 a1
      r2 :: Rslt     <- replaceRefExpr rx1 a r1
      Right (renameAddr_unsafe a1 a r2, cs)

    anAbsentPhrase :: Expr
    anAbsentPhrase = Phrase $ aap s0 where
      s0 = "&b(;HG65Lcsd.21%^!#$o6tB6*)"
      aap :: String -> String
      aap s = if isLeft $ refExprToAddr r0 (Phrase' s)
              then s else aap $ s ++ s
