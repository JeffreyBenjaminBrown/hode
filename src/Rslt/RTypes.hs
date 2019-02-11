module Rslt.RTypes where

import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes (Var)
import Util.Misc


type Addr = Int -- ^ Address
type Arity = Int

-- | = Every relationship has a "template" and some "members".
-- For instance, the relationship "dogs #like beef" has members "dogs"
-- and "beef", and template "_ like _".
data Role = RoleTplt | RoleMember Int deriving (Eq, Ord, Read, Show)


-- | = `Expr` is the fundamental type

data Expr =
    ExprAddr Addr -- ^ Refers to the `Expr` at the `Addr` in some `Rslt`.
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
    _refExprAt :: Map Addr RefExpr
  , _addrOf    :: Map RefExpr Addr
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
type RefExprs = Map Addr RefExpr


-- | = For the Hash language

-- | An `HExpr` describes a set (maybe empty) of `Expr`s in a `Rslt`.
data HExpr =
    HMap  HMap -- ^ The search workhorse.
  | HEval HMap [[Role]] -- ^ Finds matches to the `HMap`, then retrieves
  -- from each match the subexpression each `[Role]` (path) arrives at.
  -- (Inclduing more than one path in the `[[Role]]` is weird but legal.)
  | HVar   Var    -- ^ To look up the `Var` from a `Subst Addr Rslt`.
  | HExpr  Expr   -- ^ When you want exactly one `Expr`, and know which.
  -- The `ExprAddr` constructor permits referring to an `Expr` by its `Addr`.
  | HDiff HExpr HExpr -- ^ Set difference.
  | HAnd [HExpr]      -- ^ Intersection.
  | HOr  [HExpr]      -- ^ Union. Pronounced "a chore".
  deriving (Eq, Ord, Show)

-- | An `HMap` m is used to request all expressions x such that for each
-- key r in m, such that r is mapped to h, some expression in the
-- result of searching for h appears in position r in x.
-- This is not type-enforced, but to be valid an HMap  must be nonempty.
-- Searches that find nothing are easily specified -- for instance,
-- if expr x is never the second member of anything, then the `HMap`
-- `M.singleton (RoleMember 2) x` will, appropriately, find nothing.
--
-- The `Left HIt` values are ignored when evaluating the `HMap`;
-- they come into play when the `HMap` is a subexpression of some `HEval`.
type HMap = Map Role HExpr
