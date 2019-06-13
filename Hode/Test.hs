{-# LANGUAGE TupleSections #-}

module Hode.Test where

import qualified Brick.Main   as B
import qualified Brick.Types  as B
import           Brick.Util (on)
import qualified Graphics.Vty as V

import Hode.Lib


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
  showRowCol = (:[]) . (, attr1) . show . fst where
    attr1 = V.red `on` V.blue

  showRowNode :: Row -> AttrString
  showRowNode = map (, attr2) . snd where
    attr2 = V.blue `on` V.red

  rows :: [Row]
  rows = [ (123,  replicate 12 " Hi! " )
         , (4677, replicate 4  " What's up?  ")
         , (2,    replicate 4  " Later, homefries. ") ]
