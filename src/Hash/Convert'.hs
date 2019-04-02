-- | After parsing, the next step is to
-- create `HExpr`s from `PExpr`s and `PRel`s.

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.Convert' where

import           Data.Functor.Foldable
import           Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow (second)
import Data.Functor (void)

import Hash.Convert
import Hash.HLookup
import Hash.HTypes
import Hash.HUtil
import Rslt.RTypes
import Rslt.RUtil
import Util.Misc


pathsToIts_pExpr' :: PExpr -> Either String [RolePath]
pathsToIts_pExpr' (PEval pnr) = pathsToIts_sub_pExpr' pnr
pathsToIts_pExpr' x           = pathsToIts_sub_pExpr' x

pathsToIts_sub_pExpr' :: PExpr -> Either String [RolePath]
pathsToIts_sub_pExpr' = prefixLeft "pathsToIts_sub_pExpr" . para f where
  tooLate :: Base PExpr (PExpr, Either String [RolePath])
           -> Either String [RolePath]
  tooLate x = Left $ "pathsToIts_sub_pExpr called too late (too far leafward in the PExpr), on " ++ show (embed $ fmap fst x)

  f :: Base PExpr (PExpr, Either String [RolePath])
    -> Either String [RolePath]
  f (PExprF _) = Right []
  f (PMapF m)  = do (m' :: Map Role [RolePath]) <-
                      ifLefts_map "" $ M.map snd m
                    let g :: (Role, [RolePath]) -> [RolePath]
                        g (role, paths) = map ((:) role) paths
                    Right $ concatMap g $ M.toList m'
  f (PEvalF _) = Right []
    -- don't recurse into a new PEval context; the paths to
    -- that PEval's `it`s are not the path to this one's.
  f (PVarF _)        = Right []
  f x@(PDiffF _ _)   = tooLate x
  f x@(PAndF _)      = tooLate x
  f x@(POrF _)       = tooLate x
  f AnyF             = Right []
  f (ItF Nothing)    = Right [[]]
  f (ItF (Just pnr)) = fmap ([] :) $ snd pnr
  f (PParF _)        = Left "case of Par not permitted."
  f (PRelF pr)       = Right $ pathsToIts_sub_pRel pr
