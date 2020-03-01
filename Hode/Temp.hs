{-# LANGUAGE ScopedTypeVariables
#-}

module Hode.Temp where

import qualified Data.List.PointedList as P

import           Control.Lens
import qualified Data.Map as M
--import           Data.Set (Set)
--import qualified Data.Set as S
--
--import Hode.Test.Rslt.TSort
import Hode.Hode


showColumns :: ExprRow -> [ColorString]
showColumns er = let
  (inSort, selected) = _sortAndSelectColumnProps er
  in [ ( if selected then "" else "x"
       , if inSort then TextColor else SepColor ) ] :
     ( map ((:[]) . (, TextColor) . show)
         . M.elems . _numColumnProps
         $ er )

showNode :: ExprRow -> ColorString
showNode = showColor defaulViewOptions . _viewExprNode

getFolded :: ExprRow -> Bool
getFolded = _folded . _otherProps

sp :: St -> [(Bool, ColorString, ColorString)]
sp st = let
  Just (x :: PTree ExprRow) = st ^? stGetFocused_ViewExprNode_Tree . _Just
  Just (xt :: Porest ExprRow) = P.fromList [x]
  in showPorest toColorString showColumns showNode getFolded xt
