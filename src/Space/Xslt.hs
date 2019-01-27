module Space.Xslt where

import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.RTypes


data Xslt = Xslt {
    xExprs     :: Exprs
  , xImgDb     :: Map Expr Addr
  , xVarieties :: Map Addr (ExprCtr, Arity)
  , xHas       :: Map Addr (Map Role Addr)
  , xIsIn      :: Map Addr (Set (Role, Addr))
  } deriving (Show, Eq, Ord)
