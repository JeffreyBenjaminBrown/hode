module Hode.Hash.Parse.Keywords where

import Hode.Util.Misc


data RawHashSymbol = RawHashSymbol
  { _rawSymbol :: String
  , _rawSlashPrefix :: Bool }
  deriving (Show, Eq, Ord)

show' :: RawHashSymbol -> String
show' rhs =
  (if _rawSlashPrefix rhs then "/" else "")
  ++ _rawSymbol rhs

data HashKeyword = HashKeyword
  { _title :: String
  , _keyword :: String
  , _slashPrefix :: Bool
  , _help :: String }
  deriving (Show, Eq, Ord)

hash :: HashKeyword
hash = let
  rhs = RawHashSymbol { _rawSymbol = "#"
                      , _rawSlashPrefix = False }
  s = show' rhs
  in HashKeyword {
    _title = "build relationships",
    _keyword = _rawSymbol rhs,
    _slashPrefix = False,
    _help = paragraphs
      [ paragraph
        [ "The " ++ s ++ " symbol is used to define relationships."
        , "It is the fundamental operator in the Hash language."
        , "See docs/hash/the-hash-language.md for a more in-depth discussion." ]

      , paragraph
        [ "Here are some examples: "
        , "`bird " ++ s ++ "eats worm` defines a two-member \"eats\" relatinoship between bird and worm."
        , "`Bill " ++ s ++ "uses hammer " ++ s ++ "on nail` defines a three-member \"uses-on\" relationship involving Bill, hammer and nail."
        , "`Bill " ++ s ++ "eats pizza " ++ concat (replicate 2 s) ++ "because bill " ++ s ++ "cannot cook` defines a \"because\" relationship between two relationships." ] ] }
