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
    showPorest toColorString showColumns showNode getFolded
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
  => (String -> t d) -- ^ for inserting whitespace, for indentation
  -> (a -> [t d])    -- ^ Display a node's column information.
                     --   This info will be left-justified.
  -> (a -> t d)      -- ^ Display a node's payload.
                     --   This info will be indented to form a tree.
  -> (a -> Bool)     -- ^ whether to hide a node's children
  -> Porest a        -- ^ what to display
  -> [( Bool,        -- ^ whether it has focus
        t d ,        -- ^ the columns
        t d )]       -- ^ the payload

showPorest fromString showColumns showPayload isFolded p0 =
  fShow plc where

  plc :: Porest (Level, (a, [t d])) =
    fmap writeLevels $
    porestWithPaddedColumns fromString showColumns p0

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
  => (String -> t d) -- ^ will be used to inject whitespace
  -> (a -> [t d]) -- ^ how to draw the column cells at a row
  -> Porest a
  -> Porest (a, [t d])

porestWithPaddedColumns fromString makeColumns p0 = let
  p1 :: Porest (a, [t d]) = porestWith makeColumns p0
  lengths :: [Int] = maxColumnLengths $ fmap (fmap snd) p1
  leftPad :: Int -> t d -> t d
  leftPad k s = fromString (replicate (k - length s) ' ') <> s
  -- `emptyColumns` is needed because non-`ViewExpr` payloads
  -- (which are used to group `ViewExpr`s) have empty column data.
  emptyColumns :: [t d] -> [t d]
  emptyColumns [] = [fromString $ replicate (sum lengths) ' ']
  emptyColumns a = a
  in fmap ( fmap $ second $
            emptyColumns . (zipWith ($) $ map leftPad lengths)
          ) p1

-- | Computes the maximum length of each `t b`.
-- See test suite for a demo.
-- PITFALL: Assumes the lists in the input are all of equal length.
-- If some of them are instead empty (as happens with every `ExprRow`
-- with a non-`ViewExpr` payload), they are effectively ignored.
maxColumnLengths :: forall t b. Foldable t
  -- ^ Here `t d` is probably `String` or `ColorString`.
  => Porest [t b] -> [Int]
maxColumnLengths p0 = let
  p1 :: Porest [Int] =
    fmap (fmap $ map length) p0
  zeros :: [Int] =
    map (const 0)
    $ foldr1 const -- takes the first element (efficiently, I think)
    $ ( p0 ^. P.focus :: PTree [t b] )
  update :: [Int] -> [Int] -> [Int]
  update acc [] = acc
  update [] _ = [] -- this case is redundant, but GHC doesn't know that
  update (a:acc) (b:new) = max a b : update acc new
  maxima :: Foldable f => f [Int] -> [Int]
  maxima = foldr update zeros
  in maxima $ fmap maxima p1

porestWith :: (a -> [b]) -> Porest a -> Porest (a, [b])
porestWith makeColumns =
  fmap $ fmap $ \x -> (x, makeColumns x)
