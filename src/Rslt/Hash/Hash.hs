module Rslt.Hash.Hash where

import           Prelude hiding (lookup)
import           Data.Maybe (isNothing)
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.RTypes
import Rslt.Lookup
import Rslt.Hash.HTypes
import Util


hExprToExpr :: HExpr -> Either String Expr
hExprToExpr (HExpr e) = Right e
hExprToExpr h = Left $ "hExprToExpr: given " ++ show h
  ++ ", but only the HExpr constructor can be converted to an Expr.\n"


hFind :: Rslt -> HExpr -> Either String (Set Addr)

-- | TRICK: For speed, put the most selective searches first in the list.
hFind r (HAnd hs) = foldr1 S.intersection <$>
                    ( ifLefts "hFind" $ map (hFind r) hs )

hFind r (HOr hs) = foldr1 S.union <$>
                   ( ifLefts "hFind" $ map (hFind r) hs )

hFind r (HDiff base exclude) = do
  b <- prefixLeft "hFind" $ hFind r base
  e <- prefixLeft "hFind" $ hFind r exclude
  Right $ S.difference b e

hFind r (HExpr e) = S.singleton <$> lookup r e
