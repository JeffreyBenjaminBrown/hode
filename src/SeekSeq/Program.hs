{-# LANGUAGE ScopedTypeVariables #-}

module SeekSeq.Program where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import SeekSeq.Query
import SeekSeq.Query.Valid
import SeekSeq.Types
import Util


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
    go (Right p) (v,q) = do
      (ec :: CondElts e) <- prefixLeft "runProgram"
                            $ runFindlike d p (M.empty :: Subst e) q
      Right $ M.insert v ec p
