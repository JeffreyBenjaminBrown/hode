-- | Based on and simplifying digraphs-with-text/src/Dwt/Hash/Parse.hs

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash.HUtil where

import           Hash.HTypes
import           Rslt.RTypes


pnrWord :: String -> PRel
pnrWord = PNonRel . PExpr . Word
