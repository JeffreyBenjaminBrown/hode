{-# LANGUAGE TupleSections #-}

module Hode.Test where

import qualified Brick.Main   as B
import           Brick.Types
import           Brick.Util (on)
import qualified Graphics.Vty as V

import Hode.Lib


type Row = (Int, [String])

test_showTwoAspects :: IO ()
test_showTwoAspects = B.simpleMain ( showTwoAspects
  attrStringWrap showRowCol showRowNode rows :: Widget () )

test_showOneAspect :: IO ()
test_showOneAspect = B.simpleMain ( showOneAspect
  attrStringWrap showRowCol showRowNode rows :: Widget () )

showRowCol :: Row -> AttrString
showRowCol = (:[]) . (, attr1) . show . fst where
  attr1 = V.red `on` V.blue

showRowNode :: Row -> AttrString
showRowNode = map (, attr2) . snd where
  attr2 = V.blue `on` V.red

rows :: [Row]
rows = [ (100,  replicate 32 " Hi! " )
       , (123456789012345, replicate 18  " What's up?  ")
       , (2,    replicate 12  " Later, homefries. ") ]
