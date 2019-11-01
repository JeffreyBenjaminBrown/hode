{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import Hode.Hode
import Data.Functor.Foldable

-- buggy
t = eParenShow 3 (error "meh")
    $ ExprRel $ Rel [Phrase "0", Phrase "1"]
    $ ExprTplt ( Tplt
                 (Just $ Phrase "is")
                 [Phrase "is"]
                 (Just $ Phrase "is") )

-- | = Currently the show instance for `RoleHosts` is buggy,
-- because `eParenShow` (above) is buggy.

mh_t = RoleHosts
  { _memberHostsCenter = 3
  , _memberHostsRole = RoleTplt
  , _memberHostsTplt = Tplt Nothing [Phrase "is"] Nothing }

mh_1 = RoleHosts
  { _memberHostsCenter = 3
  , _memberHostsRole = RoleMember 1
  , _memberHostsTplt = Tplt Nothing [Phrase "is"] Nothing }
