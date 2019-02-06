{-# LANGUAGE ScopedTypeVariables #-}

module Rslt.RSeekSeq where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Rslt.Lookup
import Rslt.RTypes
import SeekSeq.Query.MkLeaf
import SeekSeq.Types
import Util


-- | == for building `Query`s

hFind :: HExpr -> Find Addr Rslt
hFind he = find $ flip hLookup he
