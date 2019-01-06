module Query where

import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Set (Set)
import qualified Data.Set       as S

import Types


-- | `couldBind Q = Vs` <=> `Q` could depend on a binding of any var in `Vs`.
-- `willBind` would be a nice thing to define if it were possible, but
-- (without way more data and processing) it is not.
couldBind :: Query -> Set Var
couldBind (QFind _) = S.empty
couldBind (QCond _) = S.empty
couldBind (QUnion     qs) = S.unions   $ S.map couldBind qs
couldBind (QIntersect qs) = S.unions   $ S.map couldBind qs
couldBind (ForSome vfs q) = S.union vs $       couldBind q
  where vs = S.map varFuncName vfs
couldBind (ForAll  _  q) =                     couldBind q
