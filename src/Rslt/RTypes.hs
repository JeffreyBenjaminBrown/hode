{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Rslt.RTypes where

import Data.Functor.Foldable.TH

import           Data.Map (Map)
import           Data.Set (Set)


type Addr = Int -- ^ Address
type Arity = Int

-- | = Every relationship has a "Tplt" and some "members".
-- For instance, the relationship "dogs #like beef" has members "dogs"
-- and "beef", and Tplt "_ like _".
data Role = RoleTplt | RoleMember Int deriving (Eq, Ord, Read, Show)
type RolePath = [Role] -- ^ A path to a sub-expression. For instance,
  -- if the sub-expression is the second member of the first member of the
  -- top expression, the path would be `[RoleMember 1, RoleMember 2]`.
-- | = `Expr` is the fundamental type

data Rel a = Rel [a] a
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)
type Tplt a = [a]
data Par a = Par [(String, a)] String
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)

data Expr =
    Addr Addr -- ^ Refers to the `Expr` at the `Addr` in some `Rslt`.
     -- The other `Expr` constructors are meaningful on their own, but this
    -- one requires some `Rslt` for context.
  | Phrase String   -- ^ (Could be a phrase too.)
  | ExprRel (Rel Expr) -- ^ "Relationship".
    -- The last `Addr` (the one not in the list) should be of a `Tplt`.
    -- `Rel`s are like lists in that the weird bit (`Nil|Tplt`) comes last.
  | ExprTplt (Tplt Expr) -- ^ Template for a `Rel`, ala "_ gives _ money".
                        -- The `Addr`s should probably be `Phrase`s.
  | ExprPar (Par Expr) -- ^ "Paragraph".
    -- The `String`s in a `Par` are like a single-use `Tplt`.
    -- A `Par` has Members, but (unlike a `Rel`) no `Tplt`.
    -- `Par`s are like `Tplt`s, in that |Members| + 1 = |`String`s|.
    -- `Par`s are like lists, in that the weird bit comes last.
    -- `Par` is the only kind of `RefExpr` not in the `Index`.
  deriving (Eq, Ord, Read, Show)
makeBaseFunctor ''Expr


-- | = A `Rslt` is a database of `Expr`s. It stores `RefExpr`s rather
-- than `Expr`s, for speed and compactness.

data Rslt = Rslt {
    _addrToRefExpr :: Map Addr RefExpr
  , _refExprToAddr :: Map RefExpr Addr
  , _variety       :: Map Addr (ExprCtr, Arity)
  , _has           :: Map Addr (Map Role Addr)
  , _isIn          :: Map Addr (Set (Role, Addr))
  } deriving (Eq, Ord, Read, Show)

-- | An (Expr)ession, the contents of which are (Ref)erred to via `Addr`s.
-- Unlike an `Expr`, a `RefExpr` is not meaningful on its own;
-- it requires the context of an `Rslt`.
data RefExpr =
    Phrase' String
  | Rel' (Rel Addr)
  | Tplt' (Tplt Addr)
  | Par' (Par Addr)
  deriving (Eq, Ord, Read, Show)

-- | The constructor that a `RefExpr` uses.
data ExprCtr = PhraseCtr | RelCtr | TpltCtr | ParCtr
  deriving (Eq, Ord, Read, Show)
