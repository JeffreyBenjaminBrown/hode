{-# LANGUAGE ScopedTypeVariables
, GADTs
, TemplateHaskell
, TypeFamilies
, DeriveFunctor, DeriveFoldable, DeriveTraversable
#-}

module Hode.Hash.Types where

import Data.Functor.Foldable.TH
import Data.Map (Map)

import Hode.Qseq.Types
import Hode.Rslt.Types
import Hode.Rslt.Binary


type Level = Int -- TODO Add explanatory comment.
type Separator = String

--- TODO Explain each constructor.
-- | An `HExpr` describes a set (maybe empty) of `Expr`s in a `Rslt`.
data HExpr where
  -- GADT only for the sake of adding comments to constructor arguments
  HExpr :: Expr -> HExpr
  -- ^ When you want exactly one `Expr`, and know which.
  -- The `Addr` constructor permits referring to an `Expr` by its `Addr`.
  HMap :: HMap -> HExpr -- ^ The search workhorse. See the `HMap` type.
  HMemberHosts :: HExpr -> HExpr -- ^ Things it is a member of.
  HMemberHostsRec :: Int -> HExpr -> HExpr
  HEval :: HExpr      -- ^ First, find matches to this.
        -> [RelPath] -- ^ Then, traverse into each match along these paths.
        -- Return whatever each path leads to.
        -- (Using more than one path is weird but legal.)
        -> HExpr
  HVar ::  Var -> HExpr -- ^ To look up the `Var` from a `Subst Addr Rslt`.
  HDiff :: HExpr -> HExpr -> HExpr -- ^ Set difference: things that match the first argument and not the second.
  HAnd :: [HExpr] -> HExpr -- ^ Intersection: things matching all of them.
  HOr ::  [HExpr] -> HExpr -- ^ Union: things matching any of them.
  HReach -- ^ Return all nodes that can be reached by starting somewhere and proceeding in a certain direction.
    :: SearchDir
    -> HExpr -- ^ template(s) to search along
             -- (using more than one is weird but legal)
    -> HExpr -- ^ expression(s) to start from
    -> HExpr -- ^ every `Expr` that can be reached by traversing
    -- from the starting `Expr`s along the specified `Tplt`(s) in the specified direction
  HTrans -- ^ Determine which in a set of potential start points can reach a set of potential endpoints, or which endpoints can be reached by the startpoints, or both.
    :: SearchDir -- ^ the direction in which to search
    -> [SearchDir] -- ^ whether to return left,
      -- right or both members found.
      -- (The empty list is also valid, but pointless.)
      -- Example: If searching rightward and this argument is
      -- `[SearchRightward]`,
      -- then the search will return the endpoints reached.
    -> HExpr -- ^ template(s) to search along
             -- (using more than one is weird but legal)
    -> HExpr -- ^ expression(s) to end at
    -> HExpr -- ^ expression(s) to start from
    -> HExpr
  HTplts :: HExpr -- ^ find all `Tplt`s in the `Rslt`
  deriving (Eq, Ord, Show)

-- | Defining this is hard but an example is easy.
-- Suppose `(m :: HMap) = Map.fromList [(k,v),(k',v')].
-- Then HMap represents all expressions such that
-- something matching `v` is in role `k` and something matching
-- `v'` is in role `k'`.
-- To be valid an HMap must be nonempty, but this is not type-enforced.
type HMap = Map Role HExpr


-- | = Intermediate types, on the way to parsing an `HExpr`.

-- | A `PExpr` is something that parses to an HExpr.
-- Most or all of these constructors correspond to some `HExpr` constructor.
-- Most (all?) of these constructors corresponds to a separate Hash keyword.
-- For instance, `/member` corresponds to the PMember constructor.
-- They are documented in the interactive help
-- (select "The Hash Language" from the top menu).
-- (Also, those `PExpr` contstructors (e.g. `PMap`) that correspond to an
-- `HExpr` constructor (e.g. `HMap`), if missing comments under this type,
-- probably have comments under that type.)
--
-- Most of these terms are exclusive to search.
-- If all you ever did was insert nodes into the graph,
-- `PExpr` would only need two constructors: `PExpr` and `PRel`.
--
-- PITFALL: The two types `PExpr` and `PRel` are mutually recursive.
data PExpr =
    PExpr Expr
  | PMap PMap
  | PMember PExpr
  | PInvolves Int PExpr
  | PEval PExpr
  | PVar Var
  | PDiff PExpr PExpr
  | PAnd [PExpr]
  | POr [PExpr]
  | PReach           PExpr -- ^ SearchDir implied by which member is Any
  | PTrans SearchDir PExpr
  | Any -- ^ Indicates "Don't restrict the search results by whatever occupies this spot." For instance, "goats #eat /any" would return anything goats eat.
  | It (Maybe PExpr) -- ^ Indicates "this is the spot where I want to retrieve results from." For instance, "/eval bob #flattered /it" would return all the people Bob had flattered.
  | PRel PRel
  | PTplts -- ^ All the templates in the graph.
   deriving (Eq, Show)

type PMap = Map Role PExpr


-- | A `PRel` is used to parse relationships, such as
-- "#maybe (Bob #likes Janice) ##because Janice #rescued Tom".
--
-- PITFALL: The two types `PExpr` and `PRel` are mutually recursive.
data PRel   -- ^ Intermediate type, on the way to parsing an `HExpr`
   = Absent -- ^ The leftmost and rightmost members of an `Open` or
     -- `Closed` might be absent. Interior ones should not be.
   | Closed     [PRel] [Separator] -- ^ First list: members. Second: separators.
   -- Only the first and last members can be `Absent`.
   -- |separators| + 1 = |members, including `Absent`s|
   | Open Level [PRel] [Separator] -- ^ Like `Closed`,
   -- but potentially incompletely parsed -- that is, more things
   -- might be inserted into it.
   | PNonRel PExpr
   deriving (Eq, Show)

makeBaseFunctor ''HExpr
makeBaseFunctor ''PExpr
makeBaseFunctor ''PRel
