module Space.Rslt where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.Index
import Space.Rslt.RTypes


mkRslt :: [(Addr, Expr)] -> Rslt
mkRslt pairs = (exprs, mkIndex exprs)
  where exprs = M.fromList pairs

exprAt          :: Rslt -> Addr        -> Maybe Expr
exprAt           r a = M.lookup a       $ fst r
addrOf          :: Rslt -> ImgOfExpr   -> Maybe Addr
addrOf           r   = _addrOf          $ snd r
variety         :: Rslt -> Addr        -> Maybe (ExprCtr, Arity)
variety          r   = _variety         $ snd r
positionsIn     :: Rslt -> Addr        -> Maybe (Map Role Addr)
positionsIn      r   = _positionsIn     $ snd r
positionsHeldBy :: Rslt -> Addr        -> Maybe (Set (Role,Addr))
positionsHeldBy  r   = _positionsHeldBy $ snd r
holdsPosition   :: Rslt -> (Role,Addr) -> Maybe Addr
holdsPosition    r   = _holdsPosition   $ snd r
