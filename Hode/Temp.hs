{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where

import Hode.Hode

t = ( attrStrip . attrConsolidate <$>
      ( eParenShowAttr 2 (error "") $
        ExprRel $ Rel [Phrase "love", Phrase "good"]
        $ ExprTplt $ Tplt Nothing [Phrase "is"] Nothing ) )
