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
import Query.Classify


runProgram :: Data
           -> [(Var,Query)] -- ^ queries can depend on earlier ones
           -> Either String Possible
runProgram d vqs = let
  go :: (Var, Query) -> Either String Possible -> Either String Possible
  go _ (Left s) = Left s
  go (v,q) (Right p) =
    case runFindlike d p q (M.empty :: Subst) :: Either String CondElts
    of Left s -> Left $ "runProgram: error in callee:\n" ++ s
       Right ec -> Right $ M.insert v ec p
  in foldr go (Right M.empty) vqs

--validProgram :: [(Var,Query)] -> Bool
--validProgram vqs = foldr f True vqs where
