module Rslt.RTypes where

import           Data.Map (Map)
import           Data.Set (Set)


type Addr = Int -- ^ Address
type Arity = Int

-- | = Every relationship has a "template" and some "members".
-- For instance, the relationship "dogs #like beef" has members "dogs"
-- and "beef", and template "_ like _".
data Role = RoleTplt | RoleMember Int deriving (Eq, Ord, Read, Show)


-- | = `Expr` is the fundamental type

data Expr =
    Addr Addr -- ^ Refers to the `Expr` at the `Addr` in some `Rslt`.
     -- The other `Expr` constructors are meaningful on their own, but this
    -- one requires some `Rslt` for context.
  | Word String   -- ^ (Could be a phrase too.)
  | Rel  [Expr] Expr -- ^ "Relationship".
    -- The last `Addr` (the one not in the list) should be of a `Tplt`.
    -- `Rel`s are like lists in that the weird bit (`Nil|Tplt`) comes last.
  | Tplt [Expr] -- ^ A "template" for a `Rel`, like "_ needs _ sometimes."
                 -- The `Addr`s should probably be `Word`s.
  | Par [(String, Expr)] String -- ^ "Paragraph".
    -- The `String`s in a `Par` are like a single-use `Tplt`.
    -- A `Par` has Members, but (unlike a `Rel`) no `Tplt`.
    -- `Par`s are like `Tplt`s, in that |Members| + 1 = |`String`s|.
    -- `Par`s are like lists, in that the weird bit comes last.
    -- `Par` is the only kind of `RefExpr` not in the `Index`.
  deriving (Eq, Ord, Read, Show)


-- | = A `Rslt` is a database of `Expr`s. It stores `RefExpr`s rather
-- than `Expr`s, for speed and compactness.

data Rslt = Rslt {
    _addrToRefExpr :: Map Addr RefExpr
  , _refExprToAddr    :: Map RefExpr Addr
  , _variety   :: Map Addr (ExprCtr, Arity)
  , _has       :: Map Addr (Map Role Addr)
  , _isIn      :: Map Addr (Set (Role, Addr))
  } deriving (Eq, Ord, Read, Show)

-- | An (Expr)ession, the contents of which are (Ref)erred to via `Addr`s.
-- Unlike an `Expr`, a `RefExpr` is not meaningful on its own;
-- it requires the context of an `Rslt`.
data RefExpr =
    Word' String
  | Rel' [Addr] Addr
  | Tplt' [Addr]
  | Par' [(String, Addr)] String
  deriving (Eq, Ord, Read, Show)

-- | The constructor that a `RefExpr` uses.
data ExprCtr = WordCtr | RelCtr | TpltCtr | ParCtr
  deriving (Eq, Ord, Read, Show)

-- | A `RefExprs` is used to retrieve the text of `Word`s and `Par`s.
