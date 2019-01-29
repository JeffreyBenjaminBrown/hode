{-# LANGUAGE ScopedTypeVariables #-}

module Program where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Types
import Query
import Query.Valid


runProgram :: forall e sp. (Ord e, Show e)
  => sp
  -> [(Var,Query e sp)] -- ^ ordered: `Query`s can depend on earlier ones
  -> Either String (Possible e)

runProgram d vqs = case validProgram vqs of
  Left s -> Left s
  Right () ->  foldl go (Right M.empty) vqs
    where
    go :: Either String (Possible e) -> (Var, Query e sp)
       -> Either String (Possible e)
    go (Left s) _ = Left s
    go (Right p) (v,q) =
      case runFindlike d p (M.empty :: Subst e) q
           :: Either String (CondElts e)
      of Left s -> Left $ "runProgram: error in callee:\n" ++ s
         Right ec -> Right $ M.insert v ec p
