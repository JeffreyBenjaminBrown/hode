-- TODO ? Maybe this module could be made simpler,
-- now that Tplt is more complex than a synonym for List.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show (
    eShow      -- ^        Rslt -> Expr -> Either String String
  ) where

import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.List as L

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show.Util
import Hode.Util.Misc


eShow :: Rslt -> Expr -> Either String String
eShow r = prefixLeft "eShow: " . para f where
  f :: Base Expr (Expr, Either String String) -> Either String String

  f e@(AddrF _) =
    prefixLeft ", called on Addr: "
    $ unAddr r (embed $ fmap fst e)
    >>= eShow r

  f (PhraseF w) = Right w

  f (ExprTpltF pairs) =
    prefixLeft ", called on ExprTplt: " $ do
      Tplt a bs c <- ifLefts $ fmap snd pairs
      let a' = maybe "" id a
          c' = maybe "" id c
      Right ( trimString $ concat $ L.intersperse " _ "
              $ [a'] ++ bs ++ [c'] )

  f relf@(ExprRelF (Rel ms (ExprTplt t0,_))) =
    -- The recursive argument (second member of the pair) for the Tplt
    -- is unused -- we don't need to show the whole Tplt, just its parts --
    -- and therefore not computed.
    prefixLeft ", called on ExprRel: " $ do
    t1 :: Tplt String <- ifLefts $ fmap (eShow r) t0
    mss :: [String] <- ifLefts $ map snd ms
    let rel :: Expr = embed $ fmap fst relf
        Tplt ma bs mc :: Tplt String =
          fmap (hash $ depth rel) t1
        ss :: [String] =
          maybeToList ma ++ zip' mss bs ++ maybeToList mc
    Right $ L.intercalate " " ss

  f (ExprRelF (Rel ms (a@(Addr _), _))) =
    prefixLeft ", called on Rel: " $ do
    tpltExpr <- unAddr r a
    eShow r $ ExprRel $ Rel (map fst ms) tpltExpr

  f x@(ExprRelF _) =
    Left $ ": ExprRel with non-Tplt for Tplt: "
    ++ show (embed $ fmap fst x)

