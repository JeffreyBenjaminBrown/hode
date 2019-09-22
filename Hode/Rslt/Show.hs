-- TODO ? Maybe this module could be made simpler,
-- now that Tplt is more complex than a synonym for List.

{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Rslt.Show (
    eShow      -- ^        Rslt -> Expr -> Either String String
  , eParenShow -- ^ Int -> Rslt -> Expr -> Either String String

  , hashUnlessEmptyStartOrEnd -- ^ Int -> [String] -> [String]
  , Parens(..)
  , parenExprAtDepth -- ^ Int -> Fix (ExprFWith ())
                     --       -> Fix (ExprFWith (Int,Parens))
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L
import           Data.Text (strip, pack, unpack)

import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Util.Misc
import Hode.Util.UParse


trimString :: String -> String
trimString = unpack . strip . pack

-- | `hashUnlessEmptyStartOrEnd k js` adds `k` #-marks to every joint
-- in `js`, unless it's empty and (first or last).
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

  hash :: Int -> String -> String
  hash k s = replicate k '#' ++ s

  hashUnlessEmpty :: Int -> String -> String
  hashUnlessEmpty _ "" = ""
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [String] -> [String]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss

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

  f relf@(ExprRelF (Rel ms (ExprTplt t, _))) =
  -- The recursive argument (second member of the pair) is unused, hence
  -- not computed. Instead, each joint` is `eShow`n separately.
    prefixLeft ", called on ExprRel: " $ do
    mss <- ifLefts $ map snd ms
    js <- let rel :: Expr = embed $ fmap fst relf
          in hashUnlessEmptyStartOrEnd (depth rel)
             <$> ifLefts ( tpltToList $ fmap (eShow r) t )
    Right $ trimString
      $ concatMap (\(m,j) -> m ++ " " ++ j ++ " ")
      $ zip ("" : mss) js

  f (ExprRelF (Rel ms (a@(Addr _), _))) =
    prefixLeft ", called on Rel: " $ do
    tpltExpr <- unAddr r a
    eShow r $ ExprRel $ Rel (map fst ms) tpltExpr

  f x@(ExprRelF _) =
    Left $ ": ExprRel with non-Tplt for Tplt: "
    ++ show (embed $ fmap fst x)


-- | = New style: wrapping depth-3 Exprs in parens

data Parens = InParens | Naked deriving (Show, Eq, Ord)

-- | Attaches a depth (`Int`) and a `Parens` to each `subExpr` of an `Expr`.
-- Anything `InParens` will be shown in parens.
-- The depth of a `Rel` is the maximum `nakedDepth` of its members,
-- where `nakedDepth` is like normal depth,
-- except `InParens` things contribute 0 depth.
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

eParenShow :: Int -> Rslt -> Expr -> Either String String
eParenShow maxDepth r e0 =
  prefixLeft "eParenShow: " $
  unAddrRec r e0 >>=
  fo . parenExprAtDepth maxDepth . toExprWith () where

  paren :: String -> String
  paren s = "(" ++ s ++ ")"

  f :: Fix (ExprFWith (Int,Parens)) -> Either String String
  f (Fix (EFW ((i,InParens),e))) = paren <$> g (i,e)
  f (Fix (EFW ((i,Naked)  ,e))) =            g (i,e)

  -- `fo` = `f, outermost`. For the top-level Expr,
  -- even if it has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (Int,Parens)) -> Either String String
  fo (Fix (EFW ((i,_),e))) = g (i,e)

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  g :: (Int, ExprF (Fix (ExprFWith (Int,Parens))))
    -> Either String String
  g (_, AddrF _) = Left "impossible; given earlier unAddrRec."
  g (_, PhraseF p) = Right p

  g (_, ExprTpltF js0) =
    prefixLeft "g of Tplt: " $ do
    js1 <- ifLefts $ tpltToList $ fmap f js0
    Right . concat . L.intersperse " _ " $ js1

  g (n, ExprRelF (Rel ms0 (Fix (EFW (_, ExprTpltF t))))) =
    prefixLeft "g of Rel: " $ do
    ms1 :: [String] <- ifLefts $ map f ms0
    js  :: [String] <- hashUnlessEmptyStartOrEnd n
                       <$> ifLefts (tpltToList $ fmap f t)
    Right $ unpack . strip . pack
      $ concatMap (\(m,j) -> m ++ " " ++ j ++ " ")
      $ zip ("" : ms1) js

  g (_, ExprRelF (Rel _ _)) = Left $
    "g given a Rel with a non-Tplt in the Tplt position."
