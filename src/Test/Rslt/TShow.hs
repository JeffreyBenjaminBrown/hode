{-# LANGUAGE ScopedTypeVariables #-}

module Test.Rslt.TShow where

import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set       as S
import           Test.HUnit hiding (Test)

import           Data.Rslt.RTypes
import           Data.Rslt.Show
import qualified Test.Rslt.RData as D


test_module_rslt_show = TestList [
  TestLabel "test_hashUnlessEmptyStartOrEnd" test_hashUnlessEmptyStartOrEnd
  , TestLabel "test_imgOfExpr" test_imgOfExpr
  ]

test_imgOfExpr = TestCase $ do
  assertBool "tplt" $ Right ( ImgOfTplt [ ImgOfWord ""
                                           , ImgOfWord "needs"
                                           , ImgOfWord "" ] )
    == imgOfExpr D.rslt ( Tplt [ 0, 3, 0 ] )

  assertBool "par" $ Right ( ImgOfPar [ ( "You can't eat"
                                        , ImgOfWord "oxygen" ) ]
                             "silly" )
    == imgOfExpr D.rslt ( Par [("You can't eat", 2)] "silly" )

  assertBool "rel, recursive" $
    let ti = ImgOfTplt [ ImgOfWord ""
                                                      , ImgOfWord "needs"
                                                      , ImgOfWord "" ]
    in Right ( ImgOfRel [ ImgOfWord "dog"
                        , ImgOfRel [ ImgOfWord "dog"
                                   , ImgOfWord "oxygen" ]
                          ti ]
               ti )
    == imgOfExpr D.rslt ( Rel [1,5] 4 )

test_hashUnlessEmptyStartOrEnd = TestCase $ do
  assertBool "1" $ hashUnlessEmptyStartOrEnd 2 [] == []
  assertBool "2" $ hashUnlessEmptyStartOrEnd 2 [""] == [""]
  assertBool "3" $ hashUnlessEmptyStartOrEnd 2 ["",""] == ["",""]
  assertBool "4" $ hashUnlessEmptyStartOrEnd 2 ["","",""] == ["","##",""]
  assertBool "5" $ hashUnlessEmptyStartOrEnd 2 ["a","b",""]
    == ["##a","##b",""]
  assertBool "6" $ hashUnlessEmptyStartOrEnd 2 ["","b","c"]
    == ["","##b","##c"]

a = eShow D.rslt (ImgOfWord "hello") == Right "hello"
a1 = eShow D.rslt (ImgOfTplt $ map ImgOfWord ["a","b","c"] )
  == Right "a _ b _ c"
a2 = eShow D.rslt ( ImgOfRel ( map ImgOfWord ["a","b"] )
                    $ ImgOfTplt $ map ImgOfWord ["","=",""] )
