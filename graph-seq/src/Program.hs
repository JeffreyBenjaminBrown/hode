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
runProgram d vqs = let
  go :: (Var, Query) -> Either String Possible -> Either String Possible
  go _ (Left s) = Left s
  go (v,q) (Right p) =
    case runFindlike d p q (M.empty :: Subst) :: Either String CondElts
    of Left s -> Left $ "runProgram: error in callee:\n" ++ s
       Right ec -> Right $ M.insert v ec p
  in foldr go (Right M.empty) vqs

validProgram :: [(Var,Query)] -> Either String ()
validProgram vqs = let
  doLeft (Left s) = Left $ "Invalid program:\n" ++ s
  x = foldr f (Right S.empty) vqs where
    f :: (Var, Query) -> Either String (Set Var)
                      -> Either String (Set Var)
    f _ e@(Left _) = doLeft e
    f (v,q) (Right vs) = case validQuery q of
      e@(Left _) -> doLeft e
      Right () -> Right S.empty -- TODO finish
  in either Left (Right . const ()) x
