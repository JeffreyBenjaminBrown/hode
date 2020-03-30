module Hode.Rslt.Edit (
    deleteIfUnused        -- ^ Addr
                          -- -> Rslt -> Either String Rslt
  , exprToAddrInsert      -- ^ Rslt -> Expr
                          -- -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr]
                          -- -> Either String (Rslt, [Addr])
  , insert        -- ^ RefExpr
                  -- -> Rslt -> Either String Rslt
  , insertChain   -- ^ (BinOrientation, TpltAddr) -> [Addr]
                  -- -> Rslt -> Either String Rslt
  , moveRefExpr   -- ^ Addr -> Addr
                  --  -> Rslt-> Either String Rslt
  , replaceExpr   -- ^ Expr -> Addr
                  --  -> Rslt-> Either String (Rslt, Addr)
  , replaceInRole -- ^ Role -> Addr -> Addr
                  -- -> Rslt -> Either String Rslt
  ) where

import Hode.Rslt.Edit.Terminal as X
import Hode.Rslt.Edit.AndSearch as X
import Hode.Rslt.Edit.Replace as X
import Hode.Rslt.Edit.Initial as X
