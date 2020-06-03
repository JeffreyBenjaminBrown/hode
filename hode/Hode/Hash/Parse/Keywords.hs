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

import Prelude hiding (any)
import Hode.Hash.Parse.Util
import Hode.Util.Misc



-- | * The binary operations

hash :: HashKeyword
hash = let
  hs = [ HashSymbol { _rawSymbol = "#"
                    , _slashPrefix = False } ]
  s :: String = rep 1 $ head hs
  in HashKeyword
     { _title = "build relationships"
     , _symbol = hs
     , _help = paragraphs
       [ paragraph
         [ "The " ++ s ++ " symbol is used to define relationships."
         , "It is the fundamental operator in the Hash language."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]

       , paragraph
         [ "Here are some examples: "
         , "`bird " ++ s ++ "eats worm` defines a two-member \"eats\" relatinoship between bird and worm."
         , "`Bill " ++ s ++ "uses hammer " ++ s ++ "on nail` defines a three-member \"uses-on\" relationship involving Bill, hammer and nail."
         , "`Bill " ++ s ++ "eats pizza " ++ concat (replicate 2 s) ++ "because bill " ++ s ++ "cannot cook` defines a \"because\" relationship between two relationships." ]

       , "The last example illustrates the use of symbols like `##` and `###`. These are interpreted exactly like `#`, but they bind later. For instance, `a ## b # c` is a relationship between `a` and (the relationship between `b` and `c`). If you don't like using multiple # symbols, you could instead define the order of operations with parentheses: `a # (b # c)` means the same thing. So does `a ### b ## c` -- the number of `#` characters in a hash operator is meaningful only relative to the number in other operators in the same expression."
       ] }

-- | Help for the `/eval`, `/it` and `/it=` keywords.
blurb_eval_and_it :: String
blurb_eval_and_it = paragraphs
  [ paragraph
    [ "The " ++ reph 1 eval ++ " symbol is used to extract subexpressions from superexpressions."
    , "It must be used in conjunction with the " ++ reph 1 it ++ " and " ++ reph 1 itMatching ++ " symbols."
    , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
  , "If (in the database) Bob has flattered both Alice and Chuck, then the command `/find " ++ reph 1 eval ++ " Bob #flattered " ++ reph 1 it ++ "` would return `Alice` and `Chuck`."
  , "The result of using " ++ reph 1 eval ++ " can be referred to from an outer expression. For instance, if (continuing the previous example) the database also knows that `Alice #enjoys surfing`, then `(" ++ reph 1 eval ++ " Bob #flattered " ++ reph 1 it ++ ") #enjoys " ++ reph 1 any ++ "` would return `Alice #enjoys surfing`. (It first finds both Alice and Chuck, because Bob flattered both of them. But only Alice enjoys surfing, so that's the only result.)"
  , "It is also possible to use the " ++ reph 1 itMatching ++ " keyword to restrict the possible values of " ++ reph 1 it ++ "; see the help for " ++ reph 1 itMatching ++ " for details." ]

-- | Help for the `/member` and `/involves` keywords.
blurb_member_and_involves :: String
blurb_member_and_involves = let
  inv = _rawSymbol $ head $ _symbol involves
  in paragraphs
  [ paragraph
    [ "If you'd like to find every relationship with 'salsa'"
    , "as a top-level member,"
    , "you can write '" ++ reph 1 member ++ " salsa'." ]

  , "Equivalently, you could write '" ++ reph 1 involves ++ " salsa'."

  , paragraph
    [ "If you'd like to find anything for which 'salsa'"
    , "is in one of the top two levels,"
    , "you can write '" ++ reph 2 involves ++ " salsa'."
    , "Note that `" ++ inv ++ "' is strictly more general than '"
      ++ reph 1 member ++ "'." ]

  , paragraph
    [ "You can write '/involves-k' for any positive value of 'k'."
    , "If you ask for a big value, the search might be slow." ]
  ]

example_precedence :: HashSymbol -> String
example_precedence hs = let
  one = rep 1 hs
  two = rep 2 hs
  in "The following isn't important, because you can always use parentheses instead, but the " ++ one ++ " symbol obeys the same precedence rules as # and the other binary operators. For instance, `a " ++ reph 1 hOr ++ " b " ++ two ++ " c " ++ reph 1 hash ++ " d` means the same thing as `(a " ++ reph 1 hOr ++ " b) " ++ one ++ " (c " ++ reph 1 hash ++ " d)`: Since " ++ two ++ " has two characters (the leading slash doesn't count), and the others have only one, it binds after them."

hAnd :: HashKeyword
hAnd = let
  hs = [ HashSymbol { _rawSymbol = "&"
                    , _slashPrefix = True } ]
  one = rep 1 $ head hs
  in HashKeyword
     { _title = "and"
     , _symbol = hs
     , _help = paragraphs
       [ paragraph
         [ "The " ++ one ++ " symbol represents logical conjunction in queries."
         , "That is, `a " ++ one ++
           " b` represents all expressions that match both `a` and `b`."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
       , let itHelps = reph 1 eval ++ " " ++ reph 1 it ++ " #helps"
         in "For instance, `(" ++ itHelps ++ " green people) " ++ one ++ itHelps ++ " purple people)` will find everything that helps both green people and purple people."
       , "The operator can be chained: `a " ++ one ++ " b " ++ one ++ " c " ++ one ++ " ...` will find all expressions that match `a` and `b` and `c` ..."
       , example_precedence $ head hs
       ] }

hOr :: HashKeyword
hOr = let
  hs = [ HashSymbol { _rawSymbol = "|"
                    , _slashPrefix = True } ]
  in HashKeyword
     { _title = "or"
     , _symbol = hs
     , _help = let one = rep 1 $ head hs
       in paragraphs
       [ paragraph
         [ "The " ++ one ++ " symbol represents logical disjunction in queries."
         , "That is, `a " ++ one ++
           " b` represents all expressions that match either `a` or `b`."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
       , let itHelps = reph 1 eval ++ " " ++ reph 1 it ++ " #helps"
         in "For instance, `(" ++ itHelps ++ " green people) " ++ one ++ itHelps ++ " purple people)` will find everything that helps green people or purple people or both."
       , "The operator can be chained: `a " ++ one ++ " b " ++ one ++ " c " ++ one ++ " ...` will find all expressions that match at least one of `a` or `b` or `c` or the rest."
       , example_precedence $ head hs
       ] }

diff :: HashKeyword
diff = let
  hs = [ HashSymbol { _rawSymbol = "\\"
                    , _slashPrefix = True } ]
  in HashKeyword
     { _title = "difference"
     , _symbol = hs
     , _help = let one = rep 1 $ head hs
       in paragraphs
       [ paragraph
         [ "The " ++ one ++ " symbol represents logical difference in queries."
         , "That is, `a " ++ one ++
           " b` represents all expressions that match `a` and do *not* match `b`."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
       , let itHelps = reph 1 eval ++ " " ++ reph 1 it ++ " #helps"
         in "For instance, `(" ++ itHelps ++ " green people) " ++ one ++ itHelps ++ " purple people)` will find everything that helps green people and does not help purple people."
       , example_precedence $ head hs
       ] }


-- | * The other parsers

addrs :: HashKeyword
addrs = let
  hs = hashSymbol_withSlash <$> ["@","addr"]
  one = rep 1 $ head hs
  in HashKeyword
     { _title = "address range"
     , _symbol = hs
     , _help = "The " ++ one ++ " symbol precedes a specification of expressions via their addresses. For instance, `" ++ one ++ " 1 3-5 8` represents every expression whose address is either 1, 3, 4, 5 or 8. The " ++ one ++ " symbol can be followed by any number of integers (like `3`) or integer ranges (like `3-5`)." }

any :: HashKeyword
any = let
  hs = hashSymbol_withSlash <$> [ "_", "any" ]
  one = rep 1 $ head hs
  in HashKeyword
     { _title = "anything"
     , _symbol = hs
     , _help = paragraphs
         [ "The " ++ one ++ " symbol represents anything at all. It is meaningless by itself, but meaningful as a sub-expression. For instance, `Bob #likes " ++ one ++ "` will match `Bob #likes orangutans` and `Bob #likes Picasso` and any other relationship of the form `Bob #likes <blank>`."
         , "Geeky sidenote: In some math and computer science contexts, `any` applied to an empty argument list is defined to return True. If Hode worked like that, it would return the entire graph. That would be dumb. Instead it returns nothing."
         ] }

eval :: HashKeyword
eval = let
  hs = hashSymbol_withSlash <$> ["eval","e"]
  in HashKeyword
     { _title = "find subexpression"
     , _symbol = hs
     , _help = blurb_eval_and_it }

hashKeyword :: HashKeyword
hashKeyword = let
  hs = hashSymbol_withSlash <$> [ "hash", "h" ]
  s :: String = rep 1 $ head hs
  in HashKeyword
     { _title = "maybe stupid"
     , _symbol = hs
     , _help = let
         simple = "'a # b'"
         dumb = "'" ++ s ++ " a # " ++ s ++ " b'"
         dumb2 = s ++ " (" ++ dumb ++ ")"
       in "This keyword indicates that what follows is a Hash expression. I'm not sure you ever need it. For instance, you could write " ++ dumb ++ ", or even " ++ dumb2 ++ ", but it would be easier to write " ++ simple ++ "."}

involves :: HashKeyword
involves = let
  hs = hashSymbol_withSlash <$> ["involves","i"]
  -- PITFALL: This keyword is unlike the others,
  -- in that after parsing the keyword, there should be no space,
  -- but instead a dash and then an integer.
  -- That's hard-coded in Hode/Hash/Parse.hs, and not reified in this module.
  in HashKeyword
     { _title = "with sub-expr at depth"
     , _symbol = hs
     , _help = "blurb_member_and_involves" }

it :: HashKeyword
it = let
  hs = hashSymbol_withSlash <$> [ "it" ]
  in HashKeyword
     { _title = "identify subexpression"
     , _symbol = hs
     , _help = blurb_eval_and_it }

itMatching :: HashKeyword
itMatching = let
  hs = hashSymbol_withSlash <$> [ "it=" ]
  in HashKeyword
     { _title = "id & limit subexpression"
     , _symbol = hs
     , _help = blurb_eval_and_it }

map :: HashKeyword
map = let
  hs = hashSymbol_withSlash <$> ["map","roles"]
  s :: String = rep 1 $ head hs
  in HashKeyword
  { _title = "map roles to exprs"
  , _symbol = hs
    -- PITFALL: This keyword is unlike the others,
    -- in that some parsing functionality (the "t" and integer keywords)
    -- are hard-coded in Hode/Hash/Parse.hs, and not reified in this module.
  , _help = paragraphs
    [ paragraph
      [ "Consider the command `/find " ++ s ++ " (1 a) (2 b)`."
      , "This returns all relationships for which the first member is the word \"a\""
      , "and the second is \"b\"."
      , "Notice that unlike, say, the expression `a #eats b`,"
      , "the `" ++ s ++ "` idiom lets you leave the template unspecified."
      , "It also lets you put other restrictions on the template,"
      , "specifying it somewhat but not completely." ]
    , paragraph
      [   "In addition to the keywords `1`, `2`, etc. (any positive integer),"
        , "the keyword `t` specifies the template."
        , "For instance, `" ++ s ++ " (t /t /_ is /_) (1 bill)`"
        , "is (pointlessly verbose but) equivalent to `bill #is /any`." ]
    , paragraph
      [ "What follows each of the keywords `1`, `2`, ... and `t`"
      , "can be an arbitrary Hash expression."
      , "For instance, the following identifies everything for which the template"
      , "is either the binary `is` template or the expression at `Addr 7`:"
      , "`" ++ s ++ "t /t (/_ is /_) | (/@ 7)`."
      ] ] }

member :: HashKeyword
member = let
  hs = hashSymbol_withSlash <$> ["member","m"]
  in HashKeyword
     { _title = "with top-level sub-expr"
     , _symbol = hs
     , _help = blurb_member_and_involves }

reach :: HashKeyword
reach = let
  hs = hashSymbol_withSlash <$> ["reach","tr"]
  in HashKeyword
     { _title = "transitive reach"
     , _symbol = hs
     , _help = "`/tr /_ #<= b` finds everything less than or equal to b." }

tplt :: HashKeyword
tplt = let
  hs = hashSymbol_withSlash <$> ["template","tplt","t"]
  in HashKeyword
     { _title = "template"
     , _symbol = hs
     , _help = paragraphs
       [ paragraph
         [ "Usually you'll query for phrases and relationships,"
         , "but sometimes you might want to find a specific template --"
         , "for instance, when using the `"
           ++ reph 1 Hode.Hash.Parse.Keywords.map ++ "` keyword." ]
       , paragraph
         [ "The query `" ++ reph 1 tplts
           ++ " /_ is /_` represents the binary `is` template --"
         , "the one used in relationships like 'swimming #is delicious'."
         , "Each spot for a member in the relationship is marked using the `/_` wildcard."
         , "The joints between those members can include multiple wordss:"
         , "`" ++ reph 1 tplts ++ " /_ is kind of /_`, for instance,"
         , "is the template of the relationship `chess #(is kind of) fun`." ]
       ] }

tplts :: HashKeyword
tplts = let
  hs = hashSymbol_withSlash <$> ["templates","tplts","ts"]
  in HashKeyword
     { _title = "all templates"
     , _symbol = hs
     , _help = "'" ++ reph 1 tplts ++ "' represents all the templates in the graph." }

transLeft :: HashKeyword
transLeft = let
  hs = hashSymbol_withSlash <$> ["transLeft","trl"]
  in HashKeyword
     { _title = "leftward transitive search"
     , _symbol = hs
     , _help = "If `0 #< 1 #< 2 #< 3`, then `/trl (/it= 1/|3) #< 2` will return 1 and not 3, and search leftward. If the it= was on the other side, then it would return 2, because for something in {1,3}, the relationship holds." }

transRight :: HashKeyword
transRight = let
  hs = hashSymbol_withSlash <$> ["transRight","trr"]
  in HashKeyword
     { _title = "rightward transitive search"
     , _symbol = hs
     , _help = "If `0 #< 1 #< 2 #< 3`, then `/trl (/it= 1/|3) #< 2` will return 1 and not 3, and search leftward. If the it= was on the other side, then it would return 2, because for something in {1,3}, the relationship holds." }

var :: HashKeyword
var = let
  hs = hashSymbol_withSlash <$> [ "var", "v" ]
  in HashKeyword
     { _title = "variable"
     , _symbol = hs
     , _help = "TODO ? not really implemented" }
