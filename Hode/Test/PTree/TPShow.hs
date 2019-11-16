{-# LANGUAGE ScopedTypeVariables
, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hode.Test.PTree.TPShow where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import qualified Data.List.PointedList as P

import Hode.PTree.Initial
import Hode.PTree.PShow


test_module_pTree_show :: T.Test
test_module_pTree_show = TestList [
    TestLabel "test_maxColumnLengths" test_maxColumnLengths
  , TestLabel "test_porestWithPaddedColumns" test_porestWithPaddedColumns
  ]

test_porestWithPaddedColumns :: T.Test
test_porestWithPaddedColumns = TestCase $ do
  assertBool "" $ fmap
    ( fmap (fmap snd) .
      porestWithPaddedColumns id (map show) )
    ( P.fromList
      [ PTree [1,1] True $
        P.fromList [ PTree [1,12]  True Nothing,
                     PTree [123,0]  True Nothing ]
      , PTree [0,1] True Nothing ]
      :: Maybe (Porest [Int]) )
    == ( P.fromList
         [ PTree ["  1"," 1"] True $
           P.fromList [ PTree ["  1","12"]  True Nothing,
                        PTree ["123"," 0"]  True Nothing ]
         , PTree ["  0"," 1"] True Nothing ] )
    
test_maxColumnLengths :: T.Test
test_maxColumnLengths = TestCase $ do
  assertBool "" $ fmap maxColumnLengths
    ( P.fromList [ PTree ["1","1"] True $
                   P.fromList [ PTree ["1","12"]  True Nothing,
                                PTree ["12345",""]  True Nothing ]
                 , PTree ["12","1"] True Nothing ]
      :: Maybe (Porest [String]) )
    == Just [5, 2]
