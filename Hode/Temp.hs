{-# LANGUAGE ScopedTypeVariables #-}

module Hode.Temp where


import           Data.Either
import qualified Data.Map       as M
import qualified Data.Set       as S
import           Lens.Micro hiding (has)
import           Test.HUnit

import           Hode.Rslt.RLookup hiding (exprToAddr)
import qualified Hode.Rslt.Edit         as R
import qualified Hode.Rslt.Edit.Initial as R
import qualified Hode.Rslt.Edit.Replace as R
import           Hode.Rslt.Index
import           Hode.Rslt.RTypes
import           Hode.Rslt.RValid
import           Hode.Rslt.Show
import qualified Hode.Test.Rslt.RData as D
import           Hode.Util.Misc


Right (r',as') = R.exprToAddrInsert D.rslt $
  ExprTplt [ Phrase "bar"
           , Phrase ""
           , Phrase "foo" ]

r =   fromRight (error "wut")
    $ R.insertAt 9 (Tplt' [7,0,8])
    $ fromRight (error "wut")
    $ R.insertAt 8 (Phrase' "foo")
    $ fromRight (error "wut")
    $ R.insertAt 7 (Phrase' "bar") D.rslt

as = [New 9]
