{-# LANGUAGE TupleSections #-}

module Hode.Bughunt.HandTest where

import qualified Data.List.PointedList as P

import qualified Brick.Main           as B
import qualified Brick.Types          as B

import Hode.Bughunt.Brick
import Hode.Bughunt.PTree
import Hode.Bughunt.ShowPTree


type Row = (Int, [String])

showRowCol :: Row -> AttrString
showRowCol = (:[]) . (, color1) . show . fst

showRowNode :: Row -> AttrString
showRowNode = map (, color2) . snd

test :: IO ()
test = B.simpleMain w where

  w :: B.Widget ()
  w = porestToWidget'
      attrStringWrap
      showRowCol
      showRowNode
      ( maybe (error "impossible -- non-empty list") id $
        P.fromList [pt] )

  pt :: PTree Row
  pt = PTree (0,[ "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 "
                , "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 " ]
             ) $ P.fromList
    [ PTree (1,["wh4ttup ", "wh4ttup ", "wh4ttup ", "wh4ttup "]) Nothing
    , PTree (2,["l4ter ", "l4ter ", "l4ter ", "l4ter "]) Nothing ]


t2 :: IO ()
t2 = B.simpleMain w where

  w :: B.Widget ()
  w = listToWidget'
      attrStringWrap
      showRowCol
      showRowNode
      pt

  pt :: [Row]
  pt = [ (0,[ "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 "
                , "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 ", "hi12 " ]
             )
       , (1,["wh4ttup ", "wh4ttup ", "wh4ttup ", "wh4ttup "])
       , (2,["l4ter ", "l4ter ", "l4ter ", "l4ter "]) ]
