-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HTypes where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Qseq.QTypes
import Rslt.RTypes


type Level = Int
type Joint = String

data PExpr -- ^ intermediate type, on the way to parsing a `Rel`
  = PExpr Expr
  | PMap PMap
  | PEval PExpr
  | PVar Var
  | PAnd [PExpr]
  | POr [PExpr]
  | Any
  | It (Maybe PExpr)
  | PPar [(String,PExpr)] String
  | PRel PRel
   deriving (Eq, Show)

type PMap = Map Role PExpr

data PRel -- ^ intermediate type, on the way to parsing a `Rel`
   = Absent -- ^ The leftmost and rightmost members of an `Open` or
     -- `Closed` might be absent. Interior ones should not be.
   | Closed     [PRel] [Joint] -- ^ First list: members. Second: joints.
   -- Only the first and last members can be Absent. |joints| = |members| - 1
   | Open Level [PRel] [Joint] -- ^ Like `Closed`, but more things
   -- might be inserted into it.
   | PNonRel PExpr
   deriving (Eq, Show)
