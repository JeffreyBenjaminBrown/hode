module Hode.Hash.Parse.Keywords where

import           Text.Megaparsec hiding (label)
import           Text.Megaparsec.Char

import Hode.Util.Misc
import Hode.Util.Parse


data HashSymbol = HashSymbol
  { _rawSymbol :: String
  , _slashPrefix :: Bool }
  deriving (Show, Eq, Ord)

show' :: HashSymbol -> String
show' hs =
  (if _slashPrefix hs then "/" else "")
  ++ _rawSymbol hs

rep :: Int -> HashSymbol -> String
rep n hs = (if _slashPrefix hs then "/" else "")
           ++ concat (replicate n $ _rawSymbol hs)

data HashKeyword = HashKeyword
  { _title :: String
  , _symbol :: HashSymbol
  , _help :: String }
  deriving (Show, Eq, Ord)

pThisMany :: Int -> HashKeyword -> Parser ()
pThisMany n hk = let
  hs = _symbol hk
  in string (rep n hs)
     <* notFollowedBy (string $ _rawSymbol hs)
     >> return ()

hash :: HashKeyword
hash = let
  hs = HashSymbol { _rawSymbol = "#"
                   , _slashPrefix = False }
  s = show' hs
  in HashKeyword {
    _title = "build relationships",
    _symbol = hs,
    _help = paragraphs
      [ paragraph
        [ "The " ++ s ++ " symbol is used to define relationships."
        , "It is the fundamental operator in the Hash language."
        , "See docs/hash/the-hash-language.md for a more in-depth discussion." ]

      , paragraph
        [ "Here are some examples: "
        , "`bird " ++ s ++ "eats worm` defines a two-member \"eats\" relatinoship between bird and worm."
        , "`Bill " ++ s ++ "uses hammer " ++ s ++ "on nail` defines a three-member \"uses-on\" relationship involving Bill, hammer and nail."
        , "`Bill " ++ s ++ "eats pizza " ++ concat (replicate 2 s) ++ "because bill " ++ s ++ "cannot cook` defines a \"because\" relationship between two relationships." ]

      , paragraph
        [ "The last example illustrates the use of symbols like " ++ error "TODO: illustrate ##, ### etc." ] ] }
