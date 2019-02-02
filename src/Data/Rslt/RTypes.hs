module Data.Rslt.RTypes where

import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Util


type Addr = Int -- ^ Address
type Arity = Int

data Rslt = Rslt {
    _exprAt  :: Map Addr Expr
  , _addrOf  :: Map Expr Addr
  , _variety :: Map Addr (ExprCtr, Arity)
  , _has     :: Map Addr (Map Role Addr)
  , _isIn    :: Map Addr (Set (Role, Addr))
  } deriving (Eq, Ord, Read, Show)

maxAddr :: Rslt -> Either String Addr
maxAddr = maybe errMsg Right . S.lookupMax . M.keysSet . _exprAt
  where errMsg = Left $ "maxAddr: empty Rslt.\n"

nextAddr :: Rslt -> Either String Addr
nextAddr r = (+1) <$> prefixLeft "nextAddr" (maxAddr r)

data Expr = Word String -- ^ (Could be a phrase too.)
  | Rel [Addr] Addr -- ^ "Relationship".
    -- The last `Addr` (the one not in the list) should be to a `Tplt`.
    -- `Rel`s are like lists in that the weird bit (`Nil|Tplt`) comes last.
  | Tplt [Addr] -- ^ A "template" for a `Rel`, like "_ needs _ sometimes."
                -- The `Addr`s should probably be `Word`s.
  | Par [(String, Addr)] String -- ^ "Paragraph".
    -- The `String`s in a `Par` are like a single-use `Tplt`.
    -- A `Par` has Members, but (unlike a `Rel`) no `Tplt`.
    -- `Par`s are like `Tplt`s, in that |Members| + 1 = |`String`s|.
    -- `Par`s are like lists, in that the weird bit comes last.
    -- `Par` is the only kind of `Expr` not in the `Index`.
  deriving (Eq, Ord, Read, Show)

-- | The constructor that an `Expr` uses.
data ExprCtr = Word' | Rel' | Tplt' | Par'
  deriving (Eq, Ord, Read, Show)

exprVariety :: Expr -> (ExprCtr, Arity)
exprVariety   (Word  _) = (Word', 0)
exprVariety e@(Tplt  _) = (Tplt', arity e)
exprVariety e@(Rel _ _) = (Rel' , arity e)
exprVariety e@(Par _ _) = (Par' , arity e)

data Role = RoleTplt | RoleMember Int deriving (Eq, Ord, Read, Show)

-- | Something used to locate an `Expr` in an `Index`,
-- given varying degrees of identifying information.
data ImgOfExpr = ImgOfWord String
               | ImgOfAddr Addr -- ^ Silly on its own, but useful
                                -- when nested within another ImgOfExpr.
               | ImgOfRel  [ImgOfExpr] ImgOfExpr
               | ImgOfTplt [ImgOfExpr]
               | ImgOfPar [(String, ImgOfExpr)] String
  deriving (Eq, Ord, Read, Show)

arity :: Expr -> Arity
arity (Word _)  = 0
arity (Rel x _) = length x
arity (Tplt x)  = length x - 1
arity (Par x _) = length x


-- | = An Rslt = one `Exprs` + one `Index`.
-- The index is derived from the files.

-- | The `Exprs` are used to retrieve the text of `Word`s and `Par`s.
type Exprs = Map Addr Expr -- TODO use ordinary hard-disk files
