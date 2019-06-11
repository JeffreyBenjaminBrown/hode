{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Hode.Rslt.Show' (
    eParenShow' -- ^ Int -> Rslt -> Expr -> Either String AttrString
  , hashUnlessEmptyStartOrEnd' -- ^ Int -> [AttrString] -> [AttrString]
  , sepColor, textColor -- ^ V.Attr
  ) where

import           Data.Functor.Foldable
import qualified Data.List as L

import           Brick.Util (on)
import qualified Graphics.Vty as V

import Hode.Brick
import Hode.Rslt.RLookup
import Hode.Rslt.RTypes
import Hode.Rslt.RUtil
import Hode.Rslt.Show
import Hode.Util.Misc
import Hode.Util.UParse


eParenShow' :: Int -> Rslt -> Expr -> Either String AttrString
eParenShow' maxDepth r e0 =
  prefixLeft "eParenShow: " $
  unAddrRec r e0 >>=
  fo . parenExprAtDepth maxDepth . toExprWith () where

  f :: Fix (ExprFWith (Int,Parens)) -> Either String AttrString
  f (Fix (EFW ((i,InParens),e))) = attrParen <$> g (i,e)
  f (Fix (EFW ((i,Naked)  ,e))) =                g (i,e)

  -- `fo` = `f, outermost`. For the top-level Expr,
  -- even if it has an `InParens` flag attached,
  -- it is printed without surrounding parentheses.
  fo :: Fix (ExprFWith (Int,Parens)) -> Either String AttrString
  fo (Fix (EFW ((i,_),e))) = g (i,e)

  -- PITFALL: `f` peels off the first `Parens`, not all of them.
  g :: (Int, ExprF (Fix (ExprFWith (Int,Parens))))
    -> Either String AttrString
  g (_, AddrF _) = Left "impossible; given earlier unAddrRec."
  g (_, PhraseF p) = Right [(p,textColor)]

  g (_, ExprTpltF js0) = do
    -- prefixLeft "g of Tplt: " $ do
    js1 :: AttrString <-
      concat <$> ifLefts (map f js0)
    Right $ L.intersperse (" _ ", textColor) js1

  g (n, ExprRelF (Rel ms0 (Fix (EFW (_, ExprTpltF js0))))) =
    prefixLeft "g of Rel: " $ do
    ms1 :: [AttrString] <- ifLefts $ map f ms0
    js1 :: [AttrString] <- hashUnlessEmptyStartOrEnd' n <$>
                           ifLefts (map f js0)

    let content :: [(AttrString,AttrString)] =
          zip ( ( [("",textColor)] :: AttrString )
               : ms1) js1
        space :: AttrString = [(" ", textColor)]
    Right $ attrStrip
      $ concatMap (\(m,j) -> m ++ space ++ j ++ space)
      $ content

  g (_, ExprRelF (Rel _ _)) = Left $
    "g given a Rel with a non-Tplt in the Tplt position."


-- | `hashUnlessEmptyStartOrEnd k js` adds `k` #-marks to every joint
-- in `js`, unless it's first or last and the empty string.
hashUnlessEmptyStartOrEnd' :: Int -> [AttrString] -> [AttrString]
hashUnlessEmptyStartOrEnd' k0 joints = case joints' of
  [] -> []
  s : ss ->   hashUnlessEmpty    k0 s
            : hashUnlessEmptyEnd k0 ss

  where
  joints' :: [AttrString] = map maybeParens joints where
    maybeParens :: AttrString -> AttrString
    maybeParens s = let
      s' = concatMap fst s
      in if hasMultipleWords s'
      then [("(",sepColor)] ++ s ++ [(")",sepColor)]
      else s

  hash :: Int -> AttrString -> AttrString
  hash k s = (replicate k '#', sepColor) : s

  hashUnlessEmpty :: Int -> AttrString -> AttrString
  hashUnlessEmpty _ [("",_)] = [("",textColor)]
  hashUnlessEmpty k s = hash k s

  hashUnlessEmptyEnd :: Int -> [AttrString] -> [AttrString]
  hashUnlessEmptyEnd _ [] = []
  hashUnlessEmptyEnd k [s]      =  [hashUnlessEmpty k s]
  hashUnlessEmptyEnd k (s : ss) =   hash               k s
                                  : hashUnlessEmptyEnd k ss


-- | '#' symbols and parens used to group `Expr`s are "separators".
-- (Note that ordinary text can include those symbols, too;
-- in that case they will not be colored differently.)
sepColor, textColor :: V.Attr
sepColor  = V.brightRed `on` V.black
textColor = V.brightBlue `on` V.black
