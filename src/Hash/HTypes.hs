-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HTypes where

import Qseq.QTypes
import Rslt.RTypes


type Level = Int
type Joint = String

data PRel -- ^ intermediate type, on the way to parsing a `Rel`
   = Absent -- ^ The leftmost and rightmost members of an `Open` or
     -- `Closed` might be absent. Interior ones should not be.
   | Closed     [PRel] [Joint] -- ^ First list: members. Second: joints.
   -- Only the first and last members can be Absent. |joints| = |members| - 1
   | Open Level [PRel] [Joint] -- ^ Like `Closed`, but more things
   -- might be inserted into it.
   | PNonRel PNonRel
   deriving (Eq, Show)

data PNonRel -- ^ intermediate type, on the way to parsing a `Rel`
  = PExpr Expr
  | PVar Var
  | Any
  | It (Maybe PNonRel)
  | Eval PNonRel
  | PRel PRel
   deriving (Eq, Show)
