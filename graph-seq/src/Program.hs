{-# LANGUAGE ScopedTypeVariables #-}

module Program where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Subst
import Types
import Query
import Query.Inspect


runProgram :: Data
           -> [(Var,Query)] -- ^ queries can depend on earlier ones
           -> Either String Possible
runProgram d vqs = case validProgram vqs of
  Left s -> Left s
  Right () ->  foldl go (Right M.empty) vqs
    where
    go :: Either String Possible -> (Var, Query) -> Either String Possible
    go (Left s) _ = Left s
    go (Right p) (v,q) =
      case runFindlike d p q (M.empty :: Subst) :: Either String CondElts
      of Left s -> Left $ "runProgram: error in callee:\n" ++ s
         Right ec -> Right $ M.insert v ec p


