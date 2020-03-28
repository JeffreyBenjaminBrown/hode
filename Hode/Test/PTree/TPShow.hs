{-# LANGUAGE ScopedTypeVariables
, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hode.Test.PTree.TPShow where

import qualified Test.HUnit      as T
import           Test.HUnit hiding (Test, test)

import qualified Data.List.PointedList as P

import Hode.PTree.Initial
import Hode.PTree.Show


test_module_pTree_show :: T.Test
test_module_pTree_show = TestList [
    TestLabel "test_maxColumnLengths" test_maxColumnLengths
  , TestLabel "test_porestWithPaddedColumns" test_porestWithPaddedColumns
  , TestLabel "test_showPorest" test_showPorest
  ]

test_showPorest :: T.Test
test_showPorest = TestCase $ do
  assertBool "" $
    ( fmap
      ( showPorest length id (map show) show
        ((==0) . head ) ) -- if list starts with 0, no subtree

      ( P.fromList
        [ PTree [1,1] False $
          P.fromList [ PTree [1,12]  True  Nothing,
                       PTree [123,0] False Nothing ]
        , PTree [0,1] False $
          P.fromList [ PTree [2,2] False Nothing ] ]
        :: Maybe (Porest [Int]) )
      :: Maybe [(Bool, String, String)] )

    == Just [ (False, "  1 1", "[1,1]"     )
            , (True , "  112", "  [1,12]"  )
            , (False, "123 0", "  [123,0]" )
            , (False, "  0 1", "[0,1]"     ) ]

test_porestWithPaddedColumns :: T.Test
test_porestWithPaddedColumns = TestCase $ do
  assertBool "" $ fmap
    ( fmap (fmap snd) .
      porestWithPaddedColumns length id (map show) )
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
  assertBool "" $ fmap (maxColumnLengths length)
    ( P.fromList [
        PTree              ["1",    "333","1"    ] True $
        P.fromList [ PTree ["1",    "",   "4444" ] True Nothing,
                     PTree ["55555","",   ""     ] True Nothing ]
      , PTree              ["22",   "1",  "1"    ] True Nothing ]
      :: Maybe (Porest [String]) )
    == Just [5, 3, 4]
