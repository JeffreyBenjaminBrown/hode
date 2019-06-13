{-# LANGUAGE TupleSections #-}

module Hode.Bughunt.HandTest where

import qualified Brick.Main           as B
import qualified Brick.Types          as B

import Hode.Bughunt.Brick
import Hode.Bughunt.ShowPTree


type Row = (Int, [String])

test :: IO ()
test = B.simpleMain w where

  w :: B.Widget ()
  w = showTwoAspects
      attrStringWrap
      showRowCol
      showRowNode
      rows

  showRowCol :: Row -> AttrString
  showRowCol = (:[]) . (, attr1) . show . fst

  showRowNode :: Row -> AttrString
  showRowNode = map (, attr2) . snd

  rows :: [Row]
  rows = [ (123,  replicate 12 " Hi! " )
         , (4677, replicate 4  " What's up?  ")
         , (2,    replicate 4  " Later, homefries. ") ]
