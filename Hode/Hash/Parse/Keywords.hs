module Hode.Hash.Parse.Keywords
  ( -- | * The binary operations
    hash
  , hAnd
  , hOr
  , diff

  -- | * The other parsers
  , addrs
  , Hode.Hash.Parse.Keywords.any
  , eval
  , hashKeyword
  , involves
  , it
  , itMatching
  , Hode.Hash.Parse.Keywords.map
  , member
  , reach
  , tplt
  , tplts
  , transLeft
  , transRight
  , var
  ) where

import Hode.Hash.Parse.Util
import Hode.Util.Misc



-- | * The binary operations

hash :: HashKeyword
hash = let
  hs = [ HashSymbol { _rawSymbol = "#"
                    , _slashPrefix = False } ]
  s :: String = rep 1 $ head hs
  in HashKeyword {
    _title = "build relationships",
    _symbol = hs,
    _help = paragraphs
      [ paragraph
        [ "The " ++ s ++ " symbol is used to define relationships."
        , "It is the fundamental operator in the Hash language."
        , "See docs/hash/the-hash-language.md for an in-depth discussion." ]

      , paragraph
        [ "Here are some examples: "
        , "`bird " ++ s ++ "eats worm` defines a two-member \"eats\" relatinoship between bird and worm."
        , "`Bill " ++ s ++ "uses hammer " ++ s ++ "on nail` defines a three-member \"uses-on\" relationship involving Bill, hammer and nail."
        , "`Bill " ++ s ++ "eats pizza " ++ concat (replicate 2 s) ++ "because bill " ++ s ++ "cannot cook` defines a \"because\" relationship between two relationships." ]

      , paragraph
        [ "The last example illustrates the use of symbols like " ++ error "TODO: illustrate ##, ### etc." ]
      ] }

hAnd :: HashKeyword
hAnd = let
  hs = [ HashSymbol { _rawSymbol = "&"
                    , _slashPrefix = True } ]
  s = rep 1 $ head hs
  in HashKeyword {
    _title = "and",
    _symbol = hs,
    _help = paragraphs
      [ paragraph
        [ "The " ++ s ++ " symbol is used for logical conjunction."
        , "`a " ++ s ++ " b` represents all expressions that match both `a` and `b`."
        , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
      , paragraph
        [ "Here are some examples: "
        , error "TODO: illustrate `eval` and precedence." ]
      ] }

hOr :: HashKeyword
hOr = let
  hs = [ HashSymbol { _rawSymbol = "|"
                    , _slashPrefix = True } ]
  in HashKeyword {
    _title = "or",
    _symbol = hs,
    _help = "TODO" }

diff :: HashKeyword
diff = let
  hs = [ HashSymbol { _rawSymbol = "\\"
                    , _slashPrefix = True } ]
  in HashKeyword {
    _title = "difference",
    _symbol = hs,
    _help = "TODO" }


-- | * The other parsers

addrs :: HashKeyword
addrs = let
  hs = hashSymbol_withSlash <$> ["@","addr"]
  in HashKeyword {
    _title = "address range",
    _symbol = hs,
    _help = "TODO" }

any :: HashKeyword
any = let
  hs = hashSymbol_withSlash <$> [ "_", "any" ]
  in HashKeyword {
    _title = "anything",
    _symbol = hs,
    _help = "TODO" }

eval :: HashKeyword
eval = let
  hs = hashSymbol_withSlash <$> ["eval","e"]
  in HashKeyword {
    _title = "find subexpression",
    _symbol = hs,
    _help = "TODO" }

hashKeyword :: HashKeyword
hashKeyword = let
  hs = hashSymbol_withSlash <$> [ "hash", "h" ]
  s :: String = rep 1 $ head hs
  in HashKeyword {
    _title = "maybe stupid",
    _symbol = hs,
    _help = let
      dumb = "'" ++ s ++ " a # " ++ s ++ " b'"
      dumb2 = s ++ " (" ++ dumb ++ ")"
      in "This keyword indicates that what follows is a Hash expression. I'm not sure you ever need it. For instance, you could write " ++ dumb ++ ", or even " ++ dumb2 ++ ", but it would be easier to write 'a # b'."}

involves :: HashKeyword
involves = let
  hs = hashSymbol_withSlash <$> ["involves","i"]
  -- PITFALL: This keyword is unlike the others,
  -- in that after parsing the keyword, there should be no space,
  -- but instead a dash and then an integer.
  -- That's hard-coded in Hode/Hash/Parse.hs, and not reified in this module.
  in HashKeyword {
    _title = "with sub-expr at depth",
    _symbol = hs,
    _help = "TODO" }

it :: HashKeyword
it = let
  hs = hashSymbol_withSlash <$> [ "it" ]
  in HashKeyword {
    _title = "TODO ? should not have a help title separate from `eval`",
    _symbol = hs,
    _help = "TODO ? should not have a help blurb separate from `eval`" }

itMatching :: HashKeyword
itMatching = let
  hs = hashSymbol_withSlash <$> [ "it=" ]
  in HashKeyword {
    _title = "TODO ? should not have a help title separate from `eval`",
    _symbol = hs,
    _help = "TODO ? should not have a help blurb separate from `eval`" }

map :: HashKeyword
map = let
  hs = hashSymbol_withSlash <$> ["map","roles"]
  in HashKeyword
  { _title = "map roles to exprs"
  , _symbol = hs
  , _help = -- PITFALL: This keyword is unlike the others,
    -- in that some parsing functionality (the "t" and integer keywords)
    -- are hard-coded in Hode/Hash/Parse.hs, and not reified in this module.
    "TODO" }

member :: HashKeyword
member = let
  hs = hashSymbol_withSlash <$> ["member","m"]
  in HashKeyword {
    _title = "with top-level sub-expr",
    _symbol = hs,
    _help = "TDOO" }

reach :: HashKeyword
reach = let
  hs = hashSymbol_withSlash <$> ["reach","tr"]
  in HashKeyword {
    _title = "transitive reach",
    _symbol = hs,
    _help = "`/tr /_ #<= b` finds everything less than or equal to b." }

tplt :: HashKeyword
tplt = let
  hs = hashSymbol_withSlash <$> ["template","tplt","t"]
  in HashKeyword {
    _title = "template",
    _symbol = hs,
    _help = "TODO" }

tplts :: HashKeyword
tplts = let
  hs = hashSymbol_withSlash <$> ["templates","tplts","ts"]
  in HashKeyword {
    _title = "all templates",
    _symbol = hs,
    _help = "TODO" }

transLeft :: HashKeyword
transLeft = let
  hs = hashSymbol_withSlash <$> ["transLeft","trl"]
  in HashKeyword {
    _title = "leftward transitive search",
    _symbol = hs,
    _help = "If `0 #< 1 #< 2 #< 3`, then `/trl (/it= 1/|3) #< 2` will return 1 and not 3, and search leftward. If the it= was on the other side, then it would return 2, because for something in {1,3}, the relationship holds." }

transRight :: HashKeyword
transRight = let
  hs = hashSymbol_withSlash <$> ["transRight","trr"]
  in HashKeyword {
    _title = "rightward transitive search",
    _symbol = hs,
    _help = "If `0 #< 1 #< 2 #< 3`, then `/trl (/it= 1/|3) #< 2` will return 1 and not 3, and search leftward. If the it= was on the other side, then it would return 2, because for something in {1,3}, the relationship holds." }

var :: HashKeyword
var = let
  hs = hashSymbol_withSlash <$> [ "var", "v" ]
  in HashKeyword {
    _title = "TODO ? should not have a help title",
    _symbol = hs,
    _help = "TODO ? not really implemented" }
