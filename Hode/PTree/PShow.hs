{-# LANGUAGE ScopedTypeVariables #-}

module Hode.PTree.PShow where

import           Control.Arrow (second)
import           Control.Lens hiding (Level)
import           Data.Foldable (toList)
import qualified Data.List.PointedList as P

import qualified Brick.Types          as B
import           Brick.Widgets.Core

import Hode.Hash.HTypes (Level)
import Hode.PTree.Initial


-- | To show `String`s, set `b2w = Brick.Widgets.Core.strWrap`.
-- To show `ColorString`s, use `b2s = Hode.Brick.colorStringWrap`
porestToWidget :: forall a b n.
     (b -> B.Widget n)
  -> (a -> b)      -- ^ shows the columns corresponding to each node
  -> (a -> b)      -- ^ shows the nodes that will be arranged like a tree
  -> (a -> Bool) -- ^ whether to hide a node's children
  -> (PTree a -> B.Widget n -> B.Widget n)
     -- ^ to show the focused node differently
     -- (it could be used for other stuff too)
  -> Porest a -- ^ The Porest to show
  -> B.Widget n
porestToWidget b2w showColumns showIndented isFolded style p0 =
  fShow p where

  p :: Porest (Level, a) = fmap writeLevels p0

  fShow :: Porest (Level,a) -> B.Widget n
  fShow = vBox . map recursiveWidget . toList

  recursiveWidget :: PTree (Level,a) -> B.Widget n
  recursiveWidget pt =
    oneTreeRowWidget pt <=> rest where
    rest = case pt ^. pMTrees of
             Nothing -> emptyWidget
             Just pts ->
               case isFolded $ snd $ _pTreeLabel pt of
               True -> emptyWidget
               False -> fShow pts

  oneTreeRowWidget :: PTree (Level,a) -> B.Widget n
  oneTreeRowWidget t =
    let indent :: Level   = fst $ _pTreeLabel t
        a      :: a       = snd $ _pTreeLabel t
    in style (fmap snd t) $ -- TODO ? speed:
         -- hopefully laziness implies that `snd`
         -- is applied only at the root, not throughout `t`.
         -- If not, drop everything below the root first.
       hBox
       [ b2w $ showColumns a
       , padLeft (B.Pad $ 2 * indent) $
         b2w $ showIndented a ]

showPorest :: forall a d. Monoid d
  => (String -> d) -- ^ for inserting whitespace, for indentation
  -> (a -> d)      -- ^ Display a node's column information.
                   --   This info will be left-justified.
  -> (a -> d)      -- ^ Display a node's payload.
                   --   This info will be indented to form a tree.
  -> (a -> Bool)   -- ^ whether to hide a node's children
  -> Porest a      -- ^ what to display
  -> [( Bool,      -- ^ whether it has focus
        d )]       -- ^ how it looks
showPorest fromString showColumns showPayload isFolded p0 =
  fShow p where

  p :: Porest (Level, a)
  p = fmap writeLevels p0

  fShow :: Porest (Level,a) -> [(Bool,d)]
  fShow = concatMap recursive . toList

  recursive :: PTree (Level,a) -> [(Bool,d)]
  recursive pt =
    once pt :
    case pt ^. pMTrees of
      Nothing -> []
      Just pts ->
        if isFolded $ snd $ _pTreeLabel pt
          then []
          else fShow pts

  once :: PTree (Level,a) -> (Bool, d)
  once t =
    let indent :: Level = fst $ _pTreeLabel t
        a      :: a     = snd $ _pTreeLabel t
    in ( _pTreeHasFocus t,
         showColumns a <>
         fromString (replicate (2*indent) ' ') <>
         showPayload a)

showPorest' :: forall a t d.
  (Foldable t, Monoid d, Monoid (t d))
  => (String -> t d) -- ^ for inserting whitespace, for indentation
  -> (a -> [t d])    -- ^ Display a node's column information.
                     --   This info will be left-justified.
  -> (a -> d)        -- ^ Display a node's payload.
                     --   This info will be indented to form a tree.
  -> (a -> Bool)     -- ^ whether to hide a node's children
  -> Porest a        -- ^ what to display
  -> [( Bool,        -- ^ whether it has focus
        d )]         -- ^ how it looks

showPorest' fromString showColumns showPayload isFolded p0 =
  let
  pw :: Porest (Level, (a, [t d])) =
    fmap writeLevels $
    porestWithPaddedColumns fromString showColumns p0
  in error ""

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
  in fmap (fmap $ second $ zipWith ($) $ map leftPad lengths) p1

-- | Computes the maximum length of each `t b`.
-- See test suite if that doesn't make sense.
-- PITFALL: Assumes the lists in the input are all of equal length.
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
  update [] new = [] -- this case is redundant, but GHC doesn't know that
  update (a:acc) (b:new) = max a b : update acc new
  maxima :: Foldable f => f [Int] -> [Int]
  maxima = foldr update zeros
  in maxima $ fmap maxima p1

porestWith :: (a -> [b]) -> Porest a -> Porest (a, [b])
porestWith makeColumns =
  fmap $ fmap $ \x -> (x, makeColumns x)
