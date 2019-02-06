module Rslt.RTypes where

import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Util


type Addr = Int -- ^ Address
type Arity = Int


-- | = `Expr` is the fundamental type

-- | `Expr` can be used to, among other things, locate a `RefExpr` in
-- an `Index`, given varying degrees of identifying information.
data Expr =
    ExprAddr Addr
  | Word String
  | Rel  [Expr] Expr
  | Tplt [Expr]
  | Par [(String, Expr)] String
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

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _refExprAt
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)

-- | An (Expr)ession, the contents of which are (Ref)erred to via `Addr`s.
-- Unlike an `Expr`, a `RefExpr` is not meaningful on its own;
-- it requires the context of an `Rslt`.
data RefExpr =
    Word' String     -- ^ (Could be a phrase too.)
  | Rel' [Addr] Addr -- ^ "Relationship".
    -- The last `Addr` (the one not in the list) should be of a `Tplt`.
    -- `Rel`s are like lists in that the weird bit (`Nil|Tplt`) comes last.
  | Tplt' [Addr] -- ^ A "template" for a `Rel`, like "_ needs _ sometimes."
                 -- The `Addr`s should probably be `Word`s.
  | Par' [(String, Addr)] String -- ^ "Paragraph".
    -- The `String`s in a `Par` are like a single-use `Tplt`.
    -- A `Par` has Members, but (unlike a `Rel`) no `Tplt`.
    -- `Par`s are like `Tplt`s, in that |Members| + 1 = |`String`s|.
    -- `Par`s are like lists, in that the weird bit comes last.
    -- `Par` is the only kind of `RefExpr` not in the `Index`.
  deriving (Eq, Ord, Read, Show)

-- | The constructor that a `RefExpr` uses.
data ExprCtr = WordCtr | RelCtr | TpltCtr | ParCtr
  deriving (Eq, Ord, Read, Show)

refExprVariety :: RefExpr -> (ExprCtr, Arity)
refExprVariety   (Word'  _) = (WordCtr, 0)
refExprVariety e@(Tplt'  _) = (TpltCtr, arity e)
refExprVariety e@(Rel' _ _) = (RelCtr , arity e)
refExprVariety e@(Par' _ _) = (ParCtr , arity e)

data Role = RoleTplt | RoleMember Int deriving (Eq, Ord, Read, Show)

arity :: RefExpr -> Arity
arity (Word' _)  = 0
arity (Rel' x _) = length x
arity (Tplt' x)  = length x - 1
arity (Par' x _) = length x

-- | A `RefExprs` is used to retrieve the text of `Word`s and `Par`s.
type RefExprs = Map Addr RefExpr -- TODO use ordinary hard-disk files


-- | = For the Hash language
-- An `HExpr` Describes a (possibly empty) collection of `Expr`s in a `Rslt`.
data HExpr =
    HMap  HMap -- ^ The search workhorse.
  | HEval HMap [[Role]] -- ^ Finds matches to the `HMap`, then retrieves
  -- from each match the subexpression each `[Role]` arrives at. (Inclduing
  -- more than one `[Role]` in the `[[Role]]` is weird but legal.)
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
