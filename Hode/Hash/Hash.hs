{-# LANGUAGE ViewPatterns
, ScopedTypeVariables
#-}

module Hode.Hash.Hash where

import Hode.Hash.HTypes


-- | The `hash` operator joins `Expr`s into `Rels`s. Each hash operation
-- has a level associated with it. The higher the level, the later the
-- evaluation. If a hash's arguments are both Exprs of a lower level,
-- it just starts a new Rel. If one of the Exprs is at the same level, then
-- it inserts the lower one into the other. (The only kind of Expr that has
-- a level associated with it is a Rel.)
--
-- For instance, in the expression `a #x b ##y c", the first hash will bind
-- a and b at level 1. The second hash is level two (because it includes two
-- # characters), and it must join `a # b` with `c`. These are both lower-
-- level `Expr`s (leaves like `c` have an implicit level of 0), so it
-- starts a new `Rel`.
--
-- A parenthesized `Expr` is "closed" after being parsed completely, which
-- means that it cannot be merged with another `Rel`.Thus `(a # b) # c`
-- creates two binary relationships, whereas `a # b # c` creates a single
-- ternary relationship.
--
-- The first or last member (or both, if its arity is greater than 2) of
-- a `PRel` might be `Absent`. The `PRel` is the structure we parse
-- from user input, which will then be turned into an `HExpr`. A `PRel`
-- can have absent members, because not every # has a member on both sides;
-- however the resulting `Rel` will always have a number of members
-- equal to the arity of its Tplt.
--
-- For instance, a `PRel`
-- like `Closed [Absent, Leaf "not"] ["maybe"]` becomes the `HExpr`
-- `HExpr $ Rel [n] m`, where `n` is the address of `Phrase "not"` and
-- `m` is the address of `Tplt [Phrase "maybe"]`.

hash :: Level -> Separator -> PRel -> PRel -> Either String PRel
hash l j -- ignore non-exhaustive error
  a@(isOpen -> False)
  b@(isOpen -> False)
  = startOpen l j a b

hash l j
  a@(Open l' _ _)
  b@(isOpen -> False)
  | l < l'  = Left $ "hash, with args l=" ++ show l ++ ", j=" ++ show j
              ++ ", a=" ++ show a ++ ", b=" ++ show b
              ++ ": higher level should not have been evaluated first."
  | l == l' = mergeIntoLeft j a b
  | l > l'  = startOpen l j a b

hash l j
  a@(isOpen -> False)
  b@(Open l' _ _)
  | l < l'  = Left $ "hash, with args l=" ++ show l ++ ", j=" ++ show j
              ++ ", a=" ++ show a ++ ", b=" ++ show b
              ++ ": higher level should not have been evaluated first."
  | l == l' = mergeIntoRight j a b
  | l > l'  = startOpen l j a b

-- The guards in this definition omit a few possibilities,
-- but I don't think they are reachable.
hash l j
  a@(Open la _ _)
  b@(Open lb _ _)
  | l < (min la lb) = Left
    $ "hash, with args l=" ++ show l ++ ", j=" ++ show j
    ++ ", a=" ++ show a ++ ", b=" ++ show b
    ++ ": higher level should not have been evaluated first."
  | l == la         = mergeIntoLeft j a b
  | l == lb         = mergeIntoRight j a b
  | l > (max la lb) = startOpen l j a b

hash _ _ (Open _ _ _) (Open _ _ _) = error "seems impossible."
hash _ _ _ _                       = error "impossible."


mergeIntoLeft :: Separator -> PRel -> PRel -> Either String PRel
mergeIntoLeft j (Open l mbrs separators) pr =
  Right $ Open l (mbrs ++ [pr]) (separators ++ [j])
mergeIntoLeft _ pr _ = Left $ "mergeIntoLeft: PRel " ++ show pr
  ++ " cannot receive more (if Closed) or any (if Leaf or Absent) members."

mergeIntoRight :: Separator -> PRel -> PRel -> Either String PRel
mergeIntoRight j pr (Open l mbrs separators) =
  Right $ Open l (pr:mbrs) (j:separators)
mergeIntoRight _ _ pr = Left $ "mergeIntoRight: PRel " ++ show pr
  ++ " cannot receive more (if Closed) or any (if Leaf or Absent) members."


startOpen :: Level -> Separator -> PRel -> PRel -> Either String PRel
startOpen _ _ Absent Absent =
  Left $ "startOpen called with two Absent members."
startOpen l j a b = Right $ Open l [a,b] [j]


isOpen :: PRel -> Bool
isOpen (Open _ _ _) = True
isOpen _            = False


close :: PRel -> PRel
close (Open _ mbrs js) = Closed mbrs js
close x                = x
