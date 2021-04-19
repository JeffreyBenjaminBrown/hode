{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}

module Hode.Rslt.Types where

import Data.Bifunctor.TH
import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.Eq.Deriving

import           Data.Map (Map)
import           Data.Set (Set)


type Addr = Int
type Cycle = (TpltAddr, [Addr]) -- TODO The Tplt should be binary and the Addrs are in left to right order, right?
type Arity = Int

class HasArity e where
  arity :: e -> Arity

-- | = TODO : Replace instances of the term `Addr` with these where possible.
-- TODO : Can these be reified into the type system?
-- That is, actually explaining the difference to the compiler,
-- rather than relying on the programmer to know what these are?
type RelAddr    = Addr
type MemberAddr = Addr
type TpltAddr   = Addr
type HostAddr   = Addr -- ^ something that hosts -- a `Rel` or a `Tplt`

-- | = Every relationship has a "Tplt" and some "members".
-- For instance, the relationship "dogs #like beef" has members "dogs"
-- and "beef", and the Tplt "_ like _". "dogs" is member 1, "beef" member 2.
data RoleInRel = RoleTplt | RoleMember Int
  deriving (Eq, Ord, Read, Show)

-- | In the (imaginary and dumb) template "#no _ #is _#to _ #forever",
-- the left cap would be "no", the right cap "forever",
-- the first separator "is", etc. A template need not have more than
-- one such thing -- for instance, "#maybe _" has only a left cap.
data RoleInTplt = RoleCapLeft | RoleCapRight | RoleSeparator Int
  deriving (Eq, Ord, Read, Show)

data Role = RoleInTplt' RoleInTplt | RoleInRel' RoleInRel
  deriving (Eq, Ord, Read, Show)

-- | A path to a sub-expression. For instance,
-- if the sub-expression is the second member of the first member of the
-- top expression, the path would be `[RoleMember 1, RoleMember 2]`.
-- That is, searching from the top of the superexpression corresponds
-- to reading the path left to right.
type RelPath = [RoleInRel]


-- | = `Expr` is the fundamental type.

-- ^ A `Rel` (relationship) consists of a list of members
-- and a `Tplt` (template) which describes how they relate.
data Rel a = Rel [a] a
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)
instance HasArity (Rel a) where
  arity (Rel as _) = length as
  -- Equivalently, we could get the arity from the second member,
  -- but then we'd need access to the graph,
  -- because `a` might just be an address.
deriveShow1 ''Rel
deriveEq1 ''Rel

-- ^ A `Tplt` describes a kind of first-order relationship.
-- For instance, any "_ #needs _" relationship uses
-- `Tplt Nothing ["needs"] Nothing`, because it has one (interior) separator
-- and no caps (exterior separators).
-- By contrast, any "#the _ #of _" relationship
-- would use `Tplt (Just "the") ["of"] Nothing`, because it
-- has an interior separator ("of") and a left-hand separator ("the").
-- Note that at least one of those things has to be present,
-- but it could be the empty string.
-- Elaborating a bit more of that pattern:
-- #j _         = Just j  []  Nothing
--    _ #j      = Nothing []  Just j
-- #j _ #j      = Just j  []  Just j
--    _ #j _    = Nothing [j] Nothing
-- #j _ #j _    = Just j  [j] Nothing
-- #j _ #j _ #j = Just j  [j] Just j
-- etc.
data Tplt a = Tplt (Maybe a) [a] (Maybe a)
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)
deriveShow1 ''Tplt
deriveEq1 ''Tplt
instance HasArity (Tplt a) where
  arity (Tplt _ seps _) = length seps + 1

data Expr =
    ExprAddr Addr -- ^ Refers to the `Expr` at the `Addr` in some `Rslt`.
     -- The other `Expr` constructors are meaningful on their own, but this
    -- one requires some `Rslt` for context.
  | Phrase String -- ^ Syntactically atomic units, like "is" or "Eddie Van Halen".
  | ExprRel (Rel Expr) -- ^ "Relationship". The last `Addr` in the `Rel`
    -- (the one not in the list) should be of a `Tplt`.
    -- TODO ? Can that constraint be enforced at the type level?
  | ExprTplt (Tplt Expr) -- ^ Template for a `Rel`, ala "_ uses _".
                         -- The `Addr`s should probably be `Phrase`s.
  deriving (Eq, Ord, Read, Show)
makeBaseFunctor ''Expr
deriveShow1 ''ExprF
deriveEq1 ''ExprF

-- ^ Use `ExprFWith a` with `Fix` to attach an `a`
-- to every level of an Expr:
--   import Data.Functor.Foldable (Fix)
--   x :: Fix (ExprFWith Int)
--   x = Fix $ EFW (1, ExprRelF $ Rel [...]
--                     $ Fix $ EFW (2, ...) )
newtype ExprFWith b a = EFW (b, ExprF a)
  deriving Functor
deriveBifunctor ''ExprFWith
deriveShow1 ''ExprFWith
deriveEq1 ''ExprFWith

unEFW :: ExprFWith b a -> (b, ExprF a)
unEFW (EFW ba) = ba


-- | = A `Rslt` is a database of `Expr`s. It stores `RefExpr`s rather
-- than `Expr`s, for speed and compactness.
data Rslt = Rslt {
    _addrToRefExpr :: Map Addr RefExpr
      -- ^ Given an `Addr`, tells you what's there.
      -- Every other field can be derived from this one.
  , _refExprToAddr :: Map RefExpr Addr
      -- ^ The inverse of _addrToRefExpr.
      -- TODO ? Why not use a Bimap (Dara.Relation?) instead?
  , _variety       :: Map Addr (ExprCtr, Arity)
      -- ^ Tells you what kind of thing is there.
  , _has           :: Map Addr (Map Role Addr)
      -- ^ Tells you its members, if any.
  , _isIn          :: Map Addr (Set (Role, Addr))
      -- ^ Tells you its hosts, i.e. what it belongs to.
  , _tplts         :: Set Addr
  } deriving (Eq, Ord, Read, Show)

-- | An (Expr)ession, the contents of which are (Ref)erred to via `Addr`s.
-- Whereas an `Expr` is a recursive type, a `RefExpr` is flat
-- (but each `Addr` it contains refers to another `RefExpr`).
-- Unlike an `Expr`, a `RefExpr` is not meaningful on its own;
-- it requires the context of an `Rslt`.
data RefExpr =
    Phrase' String
  | Rel' (Rel Addr)
  | Tplt' (Tplt Addr)
  deriving (Eq, Ord, Read, Show)
instance HasArity RefExpr where
  arity (Phrase' _)           = 0
  arity (Rel' (Rel x _))      = length x
  arity (Tplt' (Tplt _ js _)) = length js + 1

-- | The constructor that a `RefExpr` uses.
-- TODO ? Can I avoid this definition by using DataKinds?
data ExprCtr = PhraseCtr | RelCtr | TpltCtr
  deriving (Eq, Ord, Read, Show)
