module Hode.Rslt.Edit (
    moveRefExpr           -- ^ Addr -> Addr -> Rslt
                          -- -> Either String Rslt
  , replaceExpr           -- ^ Expr -> Addr -> Rslt
                          -- -> Either String (Rslt, Addr)
  , exprToAddrInsert      -- ^ Rslt -> Expr
                          -- -> Either String (Rslt, Addr)
  , exprToAddrInsert_list -- ^ Rslt -> [Expr]
                          -- -> Either String (Rslt, [Addr])
  , replaceInRole         -- ^ Role -> Addr -> Addr -> Rslt
                          -- -> Either String Rslt
  , deleteIfUnused        -- ^ Addr ->                 Rslt
                          -- -> Either String Rslt
  ) where

import Hode.Rslt.Edit.Terminal as X
import Hode.Rslt.Edit.AndSearch as X
import Hode.Rslt.Edit.Replace as X
import Hode.Rslt.Edit.Initial as X
