{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}

module Hode.Rslt.RTypes where

import Data.Functor.Classes
import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.Eq.Deriving

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


-- | = `Expr` is the fundamental type.

data Rel a = Rel [a] a
  deriving (Eq, Ord, Read, Show, Foldable, Functor, Traversable)
deriveShow1 ''Rel
deriveEq1 ''Rel

type Tplt a = [a]

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
  deriving (Eq, Ord, Read, Show)
makeBaseFunctor ''Expr
deriveShow1 ''ExprF
deriveEq1 ''ExprF

-- ^ `ExprFWith` example: add an Int to every level of an Expr
-- import Data.Functor.Foldable (Fix)
-- x :: Fix (ExprFWith Int)
-- x = Fix $ EFW (1 , ExprRelF $ Rel [...] $ Fix $ EFW (2, ...))
newtype ExprFWith b a = EFW (b, ExprF a)
deriveShow1 ''ExprFWith
deriveEq1 ''ExprFWith

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
  deriving (Eq, Ord, Read, Show)

-- | The constructor that a `RefExpr` uses.
data ExprCtr = PhraseCtr | RelCtr | TpltCtr
  deriving (Eq, Ord, Read, Show)
