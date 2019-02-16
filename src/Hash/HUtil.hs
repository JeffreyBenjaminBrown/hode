-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HUtil where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import           Hash.HTypes
import           Rslt.RTypes


pnrWord :: String -> PRel
pnrWord = PNonRel . PExpr . Word

pExprIsSpecific :: PExpr -> Bool
pExprIsSpecific (PMap m)       = or $ map pExprIsSpecific $ M.elems m
pExprIsSpecific (PEval px)     =          pExprIsSpecific px
pExprIsSpecific Any            = False
pExprIsSpecific (It Nothing)   = False
pExprIsSpecific (It (Just px)) =          pExprIsSpecific px
pExprIsSpecific _              = True
