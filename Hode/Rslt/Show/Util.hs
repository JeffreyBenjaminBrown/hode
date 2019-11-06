-- TODO ? Maybe this module could be made simpler,
-- now that Tplt is more complex than a synonym for List.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show.Util (
    trimString                -- ^         String -> String

  , hashUnlessEmptyStartOrEnd -- ^ Int -> [String] -> [String]
  , hash                      -- ^ Int ->  String -> String

  , Parens(..)
  , paren            -- ^ String -> String
  , parenExprAtDepth -- ^ Int -> Fix (ExprFWith ())
                     --       -> Fix (ExprFWith (Int,Parens))
  ) where

import Data.Functor.Foldable
import Data.Text (strip, pack, unpack)

import Hode.Rslt.RTypes
import Hode.Util.UParse


trimString :: String -> String
trimString = unpack . strip . pack

-- | `hashUnlessEmptyStartOrEnd k js` prefixes
-- `k` '#' characters to every joint in `js`,
-- unless it's empty and first or empty and last.
hashUnlessEmptyStartOrEnd :: Int -> [String] -> [String]
hashUnlessEmptyStartOrEnd k0 joints = case joints' of
  [] -> []
  s : ss ->   hashUnlessEmpty    k0 s
            : hashUnlessEmptyEnd k0 ss

  where
  joints' = map maybeParens joints where
    maybeParens :: String -> String
    maybeParens s = if hasMultipleWords s
      then "(" ++ s ++ ")" else s

  hashUnlessEmpty :: Int -> String -> String
  hashUnlessEmpty _ "" = ""
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [String] -> [String]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss

hash :: Int -> String -> String
hash k s = replicate k '#' ++ s


data Parens = InParens | Naked
  deriving (Show, Eq, Ord)

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- | Attaches a depth (`Int`) and a `Parens` to each `subExpr` of an `Expr`.
-- Anything `InParens` will be shown in parens.
-- The depth of a `Rel` is the maximum `nakedDepth` of its members,
-- where `nakedDepth` is like normal depth,
-- except `InParens` things contribute 0 depth.
    -- TODO ? should that be 1 instead of 0?
-- The depth of any non-`Rel` is 0.
-- A `Rel` of depth > `maxDepth` is `InParens`.
-- `Tplt`s are also `InParens` (which might not be necessary).

parenExprAtDepth :: Int -> Fix (ExprFWith ())
                        -> Fix (ExprFWith (Int,Parens))
parenExprAtDepth maxDepth = g where
  g (Fix (EFW ((), x))) = Fix $ EFW $ f x where

    f ::                ExprF (Fix (ExprFWith ()))
      -> ((Int,Parens), ExprF (Fix (ExprFWith (Int,Parens))))
    f (AddrF a)      =
      ( (0,Naked), AddrF a)
    f (PhraseF p)    =
      ( (0,Naked), PhraseF p)

    f (ExprTpltF js) =
      ( (0,InParens)
      , ExprTpltF $
        fmap (parenExprAtDepth maxDepth) js )
    f (ExprRelF (Rel ms t)) =
      ( (d, if d >= maxDepth then InParens else Naked)
      , ExprRelF $ Rel ms' $
        parenExprAtDepth maxDepth t) where
      ms' = map (parenExprAtDepth maxDepth) ms
      d = (+1) $ maximum $ map h ms' where
        h = nakedDepth . \(Fix (EFW (b,_))) -> b where
          nakedDepth :: (Int,Parens) -> Int
          nakedDepth (_,InParens) = 0
          nakedDepth (i,_) = i

