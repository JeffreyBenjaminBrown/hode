{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import Data.Functor.Foldable
import Hode.Hode

a = Addr 0
af = AddrF 0
afw = Fix $ EFW ( (), af )

e = ExprRel $ Rel
    [ ExprRel $ Rel [a, a] $ a,
      a ] $ a

f = Fix $ EFW ( (), ExprRelF $ Rel
                    [ Fix $ EFW ( (), ExprRelF $ Rel
                                      [ afw , afw ] $ afw ) ,
                      afw ] afw )
  
