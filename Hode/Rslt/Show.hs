-- TODO ? Maybe this module could be made simpler,
-- now that Tplt is more complex than a synonym for List.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show (
    X.eShow       -- ^        Rslt -> Expr -> Either String String
  , X.eParenShow  -- ^ Int -> Rslt -> Expr -> Either String String
  , Y.eParenShow' -- ^ Int -> Rslt -> Expr -> Either String AttrString
  ) where

import Hode.Rslt.Show.String as X
import Hode.Rslt.Show.AttrString as Y
