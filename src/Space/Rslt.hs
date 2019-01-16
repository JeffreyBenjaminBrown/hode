{-# LANGUAGE DataKinds #-}

module Space.Rslt where

import qualified Data.Map as M
import qualified Data.Set as S


type Addr = Int -- ^ Address
type Arity = Int
newtype Var = Var String -- ^ a variable, in the logic programming sense
  deriving (Show, Eq, Ord)

data Expr = Word String -- ^ (Could be a phrase too.)
  | Rel [Addr] Addr -- ^ "Relationship".
    -- The last `Addr` (the one not in the list) should be to a `Tplt`.
    -- `Rel`s are like lists in that the weird bit (`Nil|Tplt`) comes last.
  | Tplt [Addr] -- ^ A "template" for a `Rel`, like "_ needs _ sometimes."
                -- The `Addr`s are probably to `Word`s.
  | Par [(String, Addr)] String -- ^ "Paragraph".
    -- The `String`s in a `Par` are like a single-use `Tplt`.
    -- A `Par` has Members, but (unlike a `Rel`) no `Tplt`.
    -- `Par`s are like `Tplt`s, in that |Members| + 1 = |`String`s|.
    -- `Par`s are like lists, in that the weird bit comes last.
    -- `Par` is the only kind of `Expr` not in the `Index`.
  deriving (Show, Eq, Ord)

data Expr' = Word' | Rel' | Tplt' | Par'
  deriving (Show, Eq, Ord)

data Role = RoleTplt | RoleMember Int deriving (Show, Eq, Ord)

-- | Something used to locate an `Expr` in an `Index`,
-- given varying degrees of identifying information.
data ImgOfExpr = ImgOfExpr Expr
               | ImgOfAddr Addr
               | ImgOfRel  [ImgOfExpr] ImgOfExpr
               | ImgOfTplt [ImgOfExpr] deriving (Show, Eq, Ord)

arity :: Expr -> Arity
arity (Word _)  = 0
arity (Rel x _) = length x
arity (Tplt x)  = length x - 1
arity (Par x _) = length x


-- | = A "database" = one `Files` + one `Index`.
-- The index is derived from the files.

-- | The `Files` are used to retrieve the text of `Word`s and `Par`s.
type Files = M.Map Int Expr -- TODO use ordinary hard-disk files

-- | The `Index` can answer every fundamental connectivity question:
-- What is in something, what is something in, etc.
-- It can also find anything findable -- i.e. anything but a `Par`.
data Index = Index {
  addrOf            :: ImgOfExpr -> Maybe Addr
  , variety         :: Addr      -> Maybe (Expr', Arity)
  , positionsIn     :: Addr      -> Maybe (M.Map Role Addr)
  , positionsHeldBy :: Addr      -> Maybe (S.Set (Role, Addr))
  }

type Subst = M.Map Var Addr -- TODO ? replace `Addr` with `Either Var Addr`

data Query = QImg ImgOfExpr
   |  QHasInRole Role Query
   |  QHasInRoles [(Role, Query)]
   |  QIntersect [Query]  |  QUnion [Query]
   |  QNot Query  |  QVariety Expr'
   |  QVar String

-- | Positive `Query`s are things you can search for.
-- Non-positive `Query`s are too broad to search for.
-- `QVar` is neither positive nor negative.
isPositiveQuery, isNegativeQuery, isVariableQuery :: Query -> Bool
isPositiveQuery (QImg _)         = True
isPositiveQuery (QHasInRole _ _) = True
isPositiveQuery (QHasInRoles _)  = True
isPositiveQuery (QIntersect _)   = True
isPositiveQuery (QUnion _)       = True
isPositiveQuery _                = False
-- Will I need a recursive version?
  --isPositiveQuery (QHasInRole _ q)  =            isPositiveQuery q
  --isPositiveQuery (QHasInRoles qrs) = or  $ map (isPositiveQuery . snd) qrs
  --isPositiveQuery (QIntersect qs)   = or  $ map  isPositiveQuery        qs
  --isPositiveQuery (QUnion qs)       = and $ map  isPositiveQuery        qs

isNegativeQuery (QNot _)     = True
isNegativeQuery (QVariety _) = True
isNegativeQuery _            = False

isVariableQuery (QVar _) = True
isVariableQuery _        = False
