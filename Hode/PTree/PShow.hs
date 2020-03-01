{-# LANGUAGE ScopedTypeVariables #-}

module Hode.PTree.PShow (

  porestToWidget -- ^ (Ord n, Show n)
  -- => n                    -- ^ Brick name for the window
  -- -> (a -> [ColorString]) -- ^ to show the columns next to each payload
  -- -> (a -> ColorString)   -- ^ to show each payload
  -- -> (a -> Bool)          -- ^ is a node is hiding its children
  -- -> Maybe (Porest a)
  -- -> B.Widget n

  , oneRowWidget -- ^ (Bool, ColorString, ColorString) -> B.Widget n

  , showPorest -- ^ forall a t d.
  -- (Foldable t, Monoid (t d))
  -- -- ^ Here `t d` is probably `String` or `ColorString`.
  -- => (String -> t d) -- ^ for inserting whitespace, for indentation
  -- -> (a -> [t d])    -- ^ Display a node's column information.
  --                    --   This info will be left-justified.
  -- -> (a -> t d)      -- ^ Display a node's payload.
  --                    --   This info will be indented to form a tree.
  -- -> (a -> Bool)     -- ^ whether to hide a node's children
  -- -> Porest a        -- ^ what to display
  -- -> [( Bool,        -- ^ whether it has focus
  --       t d ,        -- ^ the columns
  --       t d )]       -- ^ the payload

  , porestWithPaddedColumns -- ^ forall a t d.
  -- -- ^ Here `t d` is probably `String` or `ColorString`.
  -- (Foldable t, Monoid (t d))
  -- => (String -> t d) -- ^ will be used to inject whitespace
  -- -> (a -> [t d]) -- ^ how to draw the column cells at a row
  -- -> Porest a
  -- -> Porest (a, [t d])

  , maxColumnLengths -- ^ forall t b. Foldable t
  -- -- ^ Here `t d` is probably `String` or `ColorString`.
  -- => Porest [t b] -> [Int]

  , transpose -- ^ [[a]] -> [[a]]
  , porestWith -- ^ (a -> [b]) -> Porest a -> Porest (a, [b])
) where

import           Control.Arrow (second)
import           Control.Lens hiding (Level)
import           Data.Foldable (toList)
import qualified Data.List.PointedList as P

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.Brick
import Hode.Brick.Wrap
import Hode.Hash.HTypes (Level)
import Hode.PTree.Initial
import Hode.PTree.Modify
import Hode.Util.Misc


porestToWidget :: (Ord n, Show n)
  => n                    -- ^ Brick name for the window
  -> (a -> [ColorString]) -- ^ to show the columns next to each payload
  -> (a -> ColorString)   -- ^ to show each payload
  -> (a -> Bool)          -- ^ whether a node is hiding its children
  -> Maybe (Porest a)
  -> B.Widget n
porestToWidget name showColumns showNode getFolded p =
  if null p
  then str "There are no buffers to show. Add one with M-S-t."
  else let
  rows :: [(Bool, ColorString, ColorString)] =
    showPorest colorStringLength toColorString showColumns showNode getFolded
    $ maybe err id p
    where
      err = error "impossible: null case handled earlier."
  in viewport name B.Vertical
     $ vBox $ map oneRowWidget rows

-- | PITFALL: `colorStringWrap` is overkill for `cols`,
-- which should be short. If `cols` is long enough to wrap,
-- there will be no room for the actual content of the node.
-- TODO ? write, use a simpler alternative to `colorStringWrap`.
oneRowWidget :: (Bool, ColorString, ColorString) -> B.Widget n
oneRowWidget (isFocused,cols,node) =
  (if isFocused then visible else id)
  $ hBox
  [ colorStringWrap 55 (isFocused, cols)
  , str " "
  , colorStringWrap 55 (isFocused, node) ]

showPorest :: forall a t d.
  (Foldable t, Monoid (t d))
  -- ^ Here `t d` is probably `String` or `ColorString`.
  => (t d -> Int) -- ^ how to compute the length of a `t d`
  -> (String -> t d) -- ^ for inserting whitespace, for indentation
  -> (a -> [t d])    -- ^ Display a node's column information.
                     --   This info will be left-justified.
  -> (a -> t d)      -- ^ Display a node's payload.
                     --   This info will be indented to form a tree.
  -> (a -> Bool)     -- ^ whether to hide a node's children
  -> Porest a        -- ^ what to display
  -> [( Bool,        -- ^ whether it has focus
        t d ,        -- ^ the columns
        t d )]       -- ^ the payload

showPorest len fromString showColumns showPayload isFolded p0 =
  fShow plc where

  plc :: Porest (Level, (a, [t d])) =
    fmap writeLevels $
    porestWithPaddedColumns len fromString showColumns p0

  fShow :: Porest (Level, (a, [t d])) -> [(Bool,t d, t d)]
  fShow = concatMap recursive . toList

  recursive :: PTree (Level, (a, [t d])) -> [(Bool, t d, t d)]
  recursive pt =
    oneNode pt :
    case pt ^. pMTrees of
      Nothing -> []
      Just pts ->
        if isFolded $ fst . snd $ _pTreeLabel pt
          then []
          else fShow pts

  oneNode :: PTree (Level,(a, [t d])) -> (Bool, t d, t d)
  oneNode t = let
    indent :: Level = fst $       _pTreeLabel t
    a      :: a     = fst $ snd $ _pTreeLabel t
    cols   :: [t d] = snd $ snd $ _pTreeLabel t
    in ( _pTreeHasFocus t
       , mconcat cols
       , fromString (replicate (2*indent) ' ') <>
         showPayload a )

porestWithPaddedColumns :: forall a t d.
  -- ^ Here `t d` is probably `String` or `ColorString`.
  (Foldable t, Monoid (t d))
  => (t d -> Int) -- ^ how to compute the length of a `t d`
  -> (String -> t d) -- ^ will be used to inject whitespace
  -> (a -> [t d]) -- ^ how to draw the column cells at a row
  -> Porest a
  -> Porest (a, [t d])
porestWithPaddedColumns len fromString makeColumns p0 = let
  p1 :: Porest (a, [t d]) = porestWith makeColumns p0
  lengths :: [Int] = maxColumnLengths len $ fmap (fmap snd) p1

  -- `leftPad` is called on each individual column
  leftPad :: Int -> t d -> t d
  leftPad k s = fromString (replicate (k - len s) ' ') <> s
  -- `rightPad` is called on the columns as a whole,
  -- because some rows don't have the same number of columnns
  rightPad :: [t d] -> [t d]
  rightPad ltd = let netLength = sum $ map len ltd
                     missing = sum lengths - netLength
    in case missing of
         0 -> ltd
         _ -> ltd ++ [fromString (replicate missing ' ')]
  in fmap ( fmap $ second $
            rightPad .
            (zipWith ($) $ map leftPad lengths)
          ) p1

-- | If `p` is a `Porest [t b]` in which the longest `[t b]`
-- has length `k`, then `maxColumnLengths len p` will return a `[Int]`
-- of length `k`. The first `Int` is the length of the longest `t b`
-- that is first in any of the `[t b]`s; the second is the longest
-- in the second position; etc. The test suite illustrates.
maxColumnLengths :: forall t b. Foldable t
  -- ^ Here `t d` is probably `String` or `ColorString`.
  => (t b -> Int) -> Porest [t b] -> [Int]
maxColumnLengths len p0 = let
  p1 :: Porest [Int] =
    fmap (fmap $ map len) p0
  ls :: [[Int]] =
    concat $ toList $ ( fmap (foldr (:) []) p1
                        :: P.PointedList [[Int]] )
  in map maximum $ transpose ls

porestWith :: (a -> [b]) -> Porest a -> Porest (a, [b])
porestWith makeColumns =
  fmap $ fmap $ \x -> (x, makeColumns x)
