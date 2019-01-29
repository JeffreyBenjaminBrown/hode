module Space.Rslt where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S

import Space.Rslt.Index
import Space.Rslt.RTypes


-- | The `Index` can answer every fundamental connectivity question:
-- What is in something, what is something in, etc.
-- It can also find anything findable -- i.e. anything but a `Par` --
-- via `addrOf`.
--
-- ^ The fields in the `Index`, plus the `holdsPosition` function,
-- are the atomic ways to search an `Rslt`.
data Index = Index {
    _addrOf          :: ImgOfExpr -> Maybe Addr
  , _variety         :: Addr      -> Maybe (ExprCtr, Arity)
  , _positionsIn     :: Addr      -> Maybe (Map Role Addr)
  , _positionsHeldBy :: Addr      -> Maybe (Set (Role, Addr))
  }

_holdsPosition :: Index -> (Role, Addr) -> Maybe Addr
_holdsPosition i (r,a) = case _positionsIn i a of
  Nothing -> Nothing
  Just ps -> M.lookup r ps

type Rslt = (Exprs, Index)


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


positionsWithinAll :: Exprs -> [(Addr, [(Role, Addr)])]
positionsWithinAll = filter (not . null . snd) . map f . M.toList where
  f :: (Addr, Expr) -> (Addr, [(Role,Addr)])
  f (a, expr) = (a, exprPositions expr)

positionsHeldByAll :: [( Addr,         [(Role, Addr)] )]
                   -> Map Addr (Set (Role, Addr))
positionsHeldByAll aras = foldl addInvertedPosition M.empty aras
