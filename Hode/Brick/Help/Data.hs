{-# LANGUAGE OverloadedStrings #-}

module Hode.Brick.Help.Data where

import qualified Data.List.PointedList as P

import Hode.Brick.Help.Types


animals, balls, abab, chemicals, furniture, cfcf :: Choice3Plist
animals =
  maybe (error "impopssible") id $
  P.fromList [ ("Apple", "Introduced evil to the world. Tasty.")
             , ( "Bird","Flies, melodious." )
             , ( "Marsupial", "Two womby phases!" )
             , ( "Snail","Slimy, fries up real nice." ) ]

balls =
  maybe (error "impopssible") id $
  P.fromList [ ("Basketball","Very bouncy.")
             , ( "Mercury","Bigger than a rugby ball, smaller than Saturn." )
             , ( "Softball","Lies! What the hell?" )
             , ( "Tennis ball", "Somehow extra awesome." ) ]

abab =
  maybe (error "impopssible") id $
  P.fromList [ ( "a", "A is for apple." )
             , ( "b", "B is for brownian motion." )
             , ( "c", "C is for Centigrade." )
             , ( "d", "D is for Darwinian." ) ]

chemicals =
  maybe (error "impopssible") id $
  P.fromList [ ("sugar", "long carbohydrate polymers")
             , ( "DMT", "illegal. Naturally manufactured by the brain." )
             , ( "capsaicin", "Intense. Probably misspelled." )
             , ( "DNA", "Hardest language ever." ) ]

furniture =
  maybe (error "impopssible") id $
  P.fromList [ ("chair","Four legs and a butt.")
             , ("Ottoman","A roomy stool.")
             , ("table", "An arrangement of cells into columns and rows.") ]

cfcf =
  maybe (error "impopssible") id $
  P.fromList [ ( "G", "G is for gyroscope." )
             , ( "H", "H is for helium." )
             , ( "I", "I is for Indonesia." )
             , ( "J", "J is for jet skis." ) ]

animals_and_balls, chemicals_and_furniture :: Choice2Plist
animals_and_balls = maybe (error "impossible") id $
                    P.fromList [ ("animals", animals)
                               , ("balls",   balls)
                               , ("abab",    abab)]

chemicals_and_furniture = maybe (error "impossible") id $
                          P.fromList [ ("chemicals", chemicals)
                                     , ("furniture", furniture)
                                     , ("cfcf",      cfcf) ]
