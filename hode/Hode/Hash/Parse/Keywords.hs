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
     { hashKeyword_name = "relate"
     , hashKeyword_symbols = hs
     , hashKeyword_help = paragraphs
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
    , "It must be used in conjunction with either the " ++ reph 1 it ++ " or the " ++ reph 1 itMatching ++ " symbol."
    , "See docs/hash/the-hash-language.md for a more in-depth discussion." ]
  , paragraph
    [ "We have already seen that if (in the database)"
    , "bob has flattered both alice and chuck,"
    , "then `/find bob #flattered /it` would return both `bob #flattered alice`"
    , "and `bob #flattered chuck`."
    , "That is, it returns two `_ flattered _` relationships."
    ]

  , paragraph
    [ "What if we don't want those relationships,"
    , "but instead just their right-hand members `alice` and `chuck`?"
    , "That's what `/eval` is for. `/find /eval bob #flattered /it`"
    , "would return two people, not two `_ flattered _` relationships." ]

  , paragraph
    [ reph 1 eval ++ " expressions do not have to be top-level --"
    , "the results of " ++ reph 1 eval ++ " can be referred to by an outer expression."
    , "For instance, if (continuing the previous example) the database also knows that `Alice #enjoys surfing`, then `(" ++ reph 1 eval ++ " Bob #flattered " ++ reph 1 it ++ ") #enjoys " ++ reph 1 any ++ "` would return `Alice #enjoys surfing`."
    , "The parenthesized part matches both Alice and Chuck, because Bob flattered both of them."
    , "But only Alice enjoys surfing, so that's the only expression returned." ]
  , paragraph
    [ "It is also possible to use the " ++ reph 1 itMatching ++ " keyword to restrict the possible values considered by " ++ reph 1 eval ++ "."
    , "For instance, suppose you need to know whether Jane or Jim is invited to your wedding, and you don't want to be bothered with the names of anybody else."
    , "You could in that case ask Hode to find `" ++ reph 1 eval ++ " (" ++ reph 1 itMatching ++ " Jane " ++ reph 1 hOr ++ " Jim) #is invited to my wedding`."
    , "If only Jim and Alice are invited, this will return Jim, but not Jane (because she's not invited) and not Alice (because she was not specified in the query)." ] ]

-- | Help for the `/member` and `/involves` keywords.
blurb_member_and_involves :: String
blurb_member_and_involves = let
  in paragraphs
  [ "The " ++ reph 1 member ++ " and " ++ reph 1 involves ++ " keywords are similar. Both let you find the set of expressions containing some sub-expression, without specifying precisely where the sub-expression should be."
  , paragraph
    [ "For instance, to indicate every relationship with 'salsa'"
    , "as a top-level member,"
    , "you can write '" ++ reph 1 member ++ " salsa'."
    , "This will find `salsa #has tomatoes` and `Jenny #hates salsa` and `I #buy salsa #from Trader Joe's`."
    , "It will not return `salsa`, because that's not a relationship."
    ]

  , paragraph
    [ reph 1 member ++ " only searches for top-level members."
    , "Therefore, '" ++ reph 1 member ++ "salsa' will not find `Jenny #is (allergic #to salsa)`, because salsa is not a top-level member of that relationship -- it is a level-2 member."
    ]
  , paragraph
    [ "If you want to include more than top-level members, you can -- that's what " ++ reph 1 involves ++ " is for."
    , "The " ++ reph 1 involves ++ " keyword is similar to " ++ reph 1 member ++ ", but more general."
    , "Whereas " ++ reph 1 member ++ " only allows you to search for top-level members, " ++ reph 1 involves ++ " lets you search the top level, or the top two levels, or the top three, etc."
    , "Returning to our example, if you'd like to find anything for which 'salsa'"
    , "is in one of the top two levels,"
    , "you can write '" ++ reph 1 involves ++ "-2 salsa'." ]

  , paragraph
    [ "You can write '" ++ reph 1 involves ++ "-k' for any positive value of 'k'."
    , "If you ask for a big value, the search might be slow." ]
  ]

trans_blurb :: String
trans_blurb = paragraphs
       [ paragraphs
         [ reph 1 transRight ++ " and " ++ reph 1 transLeft ++ " are almost identical."
         , "First I'll explain how to use " ++ reph 1 transRight ++ "; then I'll explain how it differs from " ++ reph 1 transLeft ++ "." ]
       , paragraph
         [ reph 1 transRight ++ " is used to find which of a specified set of things lies on one side or the other of another expression."
         , "For instance, consider a graph with a transitive `#<` template, containing the following relationships: "
         , "0 #< 1."
         , "1 #< 2."
         , "2 #< 3."
         , "3 #< 4."
         , "Suppose we want to know whether 0 or 4 is less than 2."
         , "(Neither 0 nor 4 is connected directly to 2, but transitivity allows us to infer their relationships to it.)"
         , "The search `" ++ reph 1 transRight ++ " (" ++ reph 1 itMatching ++ " 0 " ++ reph 1 hOr ++ " 4) #< 2` will return `0` and not `4`, because (transitively) `0 #< 2`, and `4` is not."
         , "The search could be translated into English as `show me which expressions in the set {0,4} are less than 2`."]
       , paragraph
         [ "Note that we must use " ++ reph 1 itMatching ++ " to identify which side of the relationship to return results from."
         , "If we had put " ++ reph 1 itMatching ++ " on the other side, as in `" ++ reph 1 transRight ++ " ( 0 " ++ reph 1 hOr ++ " 4) #< " ++ reph 1 itMatching ++ "2`, then the search would return 2, because 0 < 2."
         , "This second search could be translated as `show me which expressions in the set {2} is greater than something in the set {0, 4}`." ]
       , paragraph
         [ reph 1 transLeft ++ " searches leftward (from right to left), while " ++ reph 1 transRight ++ " searches rightward (from left to right)."
         , "Both give the same results, but one might be faster than the other."
         , "Consider for instance the search `" ++ reph 1 transLeft ++ " (" ++ reph 1 itMatching ++ " a " ++ reph 1 hOr ++ " b) #is (c " ++ reph 1 hOr ++ " d)`."
         , "This will start by finding `a` and `b`, and then see if it can reach `c` or `d`."
         , "If we use " ++ reph 1 transRight ++ ", Hode will instead start by finding `c` and `d`, and then work leftward to see if it can reach `a` or `b` from there."
         , "Note that " ++ reph 1 itMatching ++ " does not determine the direction of search; it only determines which side is returned as the result of the search."
         , "Both of the previous searches will return some subset of the set {a,b}."
         ] ]

example_precedence :: HashSymbol -> String
example_precedence hs = let
  one = rep 1 hs
  two = rep 2 hs
  in "If you don't like using parentheses to control the order in which binary operators operate, you can avoid them. The " ++ one ++ " symbol obeys the same precedence rules as # and the other binary operators. For instance, `a " ++ reph 1 hOr ++ " b " ++ two ++ " c " ++ reph 1 hash ++ " d` means the same thing as `(a " ++ reph 1 hOr ++ " b) " ++ one ++ " (c " ++ reph 1 hash ++ " d)`: Since " ++ two ++ " has two characters (the leading slash doesn't count), and the others have only one, " ++ two ++ " binds after them."

hAnd :: HashKeyword
hAnd = let
  hs = [ HashSymbol { _rawSymbol = "&"
                    , _slashPrefix = True } ]
  one = rep 1 $ head hs
  in HashKeyword
     { hashKeyword_name = "and"
     , hashKeyword_symbols = hs
     , hashKeyword_help = paragraphs
       [ paragraph
         [ "The " ++ one ++ " symbol represents logical conjunction in queries."
         , "That is, `a " ++ one ++
           " b` represents all expressions that match both `a` and `b`."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
       , let itHelps = reph 1 eval ++ " " ++ reph 1 it ++ " #helps"
         in "For instance, `(" ++ itHelps ++ " green people) " ++ one ++ " (" ++ itHelps ++ " purple people)` will find everything that helps both green people and purple people."
       , "The operator can be chained: `a " ++ one ++ " b " ++ one ++ " c " ++ one ++ " ...` will find all expressions that match `a` and `b` and `c` ..."
       , example_precedence $ head hs
       ] }

hOr :: HashKeyword
hOr = let
  hs = [ HashSymbol { _rawSymbol = "|"
                    , _slashPrefix = True } ]
  in HashKeyword
     { hashKeyword_name = "or"
     , hashKeyword_symbols = hs
     , hashKeyword_help = let one = rep 1 $ head hs
       in paragraphs
       [ paragraph
         [ "The " ++ one ++ " symbol represents logical disjunction in queries."
         , "That is, `a " ++ one ++
           " b` represents all expressions that match either `a` or `b` (or both)."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
       , let itHelps = reph 1 eval ++ " " ++ reph 1 it ++ " #helps"
         in "For instance, `(" ++ itHelps ++ " green people) " ++ one ++ " (" ++ itHelps ++ " purple people)` will find everything that helps green people or purple people or both."
       , "The operator can be chained: `a " ++ one ++ " b " ++ one ++ " c " ++ one ++ " ...` will find all expressions that match at least one of `a` or `b` or `c` or the rest."
       , example_precedence $ head hs
       ] }

diff :: HashKeyword
diff = let
  hs = [ HashSymbol { _rawSymbol = "\\"
                    , _slashPrefix = True } ]
  in HashKeyword
     { hashKeyword_name = "difference"
     , hashKeyword_symbols = hs
     , hashKeyword_help = let one = rep 1 $ head hs
       in paragraphs
       [ paragraph
         [ "The " ++ one ++ " symbol represents logical difference in queries."
         , "That is, `a " ++ one ++
           " b` represents all expressions that match `a` and do *not* match `b`."
         , "See docs/hash/the-hash-language.md for an in-depth discussion." ]
       , let itHelps = reph 1 eval ++ " " ++ reph 1 it ++ " #helps"
         in "For instance, `(" ++ itHelps ++ " green people) " ++ one ++ " (" ++ itHelps ++ " purple people)` will find everything that helps green people and does not help purple people."
       , example_precedence $ head hs
       ] }


-- | * The other parsers

addrs :: HashKeyword
addrs = let
  hs = hashSymbol_withSlash <$> ["@","addr"]
  one = rep 1 $ head hs
  in HashKeyword
     { hashKeyword_name = "address range"
     , hashKeyword_symbols = hs
     , hashKeyword_help = paragraph
       [ "The " ++ one ++ " symbol precedes a specification of expressions via their addresses."
       , "For instance, `" ++ one ++ " 1 3-5 8` represents every expression whose address is either 1, 3, 4, 5 or 8."
       , "The " ++ one ++ " symbol can be followed by any number of integers (like `3`) and integer ranges (like `3-5`)."
       , "They don't have to be in order."
       ] }

any :: HashKeyword
any = let
  hs = hashSymbol_withSlash <$> [ "_", "any" ]
  one = rep 1 $ head hs
  in HashKeyword
     { hashKeyword_name = "anything"
     , hashKeyword_symbols = hs
     , hashKeyword_help = paragraphs
         [ "The " ++ one ++ " symbol is a `wildcard`: it represents anything at all. It is meaningless by itself [see footnote], but useful as a sub-expression."
         , "For instance, `Bob #likes " ++ one ++ "` will match `Bob #likes orangutans` and `Bob #likes (diving #for doughnuts)` and any other relationship of the form `Bob #likes <blank>`."
         , "=================="
         , "Footnote: It did not have to be meaningless by itself. Hode could have been written so that if you asked for `/_`, it would return everything in your graph. But you probably wouldn't want that, and it might crash your computer."
         ] }

eval :: HashKeyword
eval = let
  hs = hashSymbol_withSlash <$> ["eval","e"]
  in HashKeyword
     { hashKeyword_name = "find expr, return subexpr"
     , hashKeyword_symbols = hs
     , hashKeyword_help = blurb_eval_and_it }

hashKeyword :: HashKeyword
hashKeyword = let
  hs = hashSymbol_withSlash <$> [ "hash", "h" ]
  s :: String = rep 1 $ head hs
  in HashKeyword
     { hashKeyword_name = "maybe stupid"
     , hashKeyword_symbols = hs
     , hashKeyword_help = let
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
  { hashKeyword_name = "with sub-expr at depth"
  , hashKeyword_symbols = hs
  , hashKeyword_help = blurb_member_and_involves }

it :: HashKeyword
it = let
  hs = hashSymbol_withSlash <$> [ "it" ]
  in HashKeyword
  { hashKeyword_name = "id subexpr in expr"
  , hashKeyword_symbols = hs
  , hashKeyword_help = blurb_eval_and_it }

itMatching :: HashKeyword
itMatching = let
  hs = hashSymbol_withSlash <$> [ "it=" ]
  in HashKeyword
  { hashKeyword_name = "id & limit subexpr in expr"
  , hashKeyword_symbols = hs
  , hashKeyword_help = "The " ++ reph 1 itMatching ++ " keyword is used in two contexts: " ++ reph 1 eval ++ " and " ++ reph 1 transLeft ++ " (or " ++ reph 1 transRight ++ "). See the documentation regarding those keywords for details." }

map :: HashKeyword
map = let
  hs = hashSymbol_withSlash <$> ["map","roles"]
  s :: String = rep 1 $ head hs
  in HashKeyword
  { hashKeyword_name = "map roles to exprs"
  , hashKeyword_symbols = hs
    -- PITFALL: This keyword is unlike the others,
    -- in that some parsing functionality (the "t" and integer keywords)
    -- are hard-coded in Hode/Hash/Parse.hs, and not reified in this module.
  , hashKeyword_help = paragraphs
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
     { hashKeyword_name = "with top-level sub-expr"
     , hashKeyword_symbols = hs
     , hashKeyword_help = blurb_member_and_involves }

tplt :: HashKeyword
tplt = let
  hs = hashSymbol_withSlash <$> ["template","tplt","t"]
  in HashKeyword
     { hashKeyword_name = "template"
     , hashKeyword_symbols = hs
     , hashKeyword_help = paragraphs
       [ paragraph
         [ "Usually you'll query for phrases and relationships,"
         , "but sometimes you might want to find a specific template --"
         , "for instance, when using the `"
           ++ reph 1 Hode.Hash.Parse.Keywords.map ++ "` keyword." ]
       , paragraph
         [ "The query `" ++ reph 1 tplt
           ++ " /_ is /_` represents the binary `is` template --"
         , "the one used in relationships like 'swimming #is delicious'."
         , "Each spot for a member in the relationship is marked using the `/_` wildcard."
         , "The separators between those members can include multiple words:"
         , "`" ++ reph 1 tplt ++ " /_ is kind of /_`, for instance,"
         , "is the template of the relationship `chess #(is kind of) fun`." ]
       ] }

tplts :: HashKeyword
tplts = let
  hs = hashSymbol_withSlash <$> ["templates","tplts","ts"]
  in HashKeyword
     { hashKeyword_name = "all templates"
     , hashKeyword_symbols = hs
     , hashKeyword_help = "'" ++ reph 1 tplts ++ "' represents all the templates in the graph." }

reach :: HashKeyword
reach = let
  hs = hashSymbol_withSlash <$> ["reach","tr"]
  in HashKeyword
     { hashKeyword_name = "transitive reach"
     , hashKeyword_symbols = hs
     , hashKeyword_help = paragraphs
       [ paragraph
         [ "The `" ++ reph 1 reach ++ "` keyword is used to find everything on one side or the other of some expression, where `side` is defined in terms of some transitive relationship."
         , "For instance, suppose the graph contains `0 #<= 1`, `1 #<= 2` and `2 #<=3`."
         , "In that case, `" ++ reph 1 reach ++ " " ++ reph 1 any ++ " #<= b` would find everything less than or equal to `b`."
         , "Notice the importance of the position of the " ++ reph 1 any ++ " in that expression."
         , "If we reversed it, to `" ++ reph 1 reach ++ " b #<= " ++ reph 1 any ++ "`, we would instead get everything that `b` is less than or equal to." ]
       , paragraph
         [ "Here's a less numerical example: `" ++ reph 1 reach ++ " (animals " ++ reph 1 hOr ++ " plants) #need " ++ reph 1 any ++ "`` would find everything that plants or animals need, recursively."
         , "If either one of them needs dirt, and dirt needs atmospheric shielding, then dirt and atmospheric shielding will both be in the results." ]
       ] }

transLeft :: HashKeyword
transLeft = let
  hs = hashSymbol_withSlash <$> ["transLeft","trl"]
  in HashKeyword
     { hashKeyword_name = "left transitive search"
     , hashKeyword_symbols = hs
     , hashKeyword_help = trans_blurb }

transRight :: HashKeyword
transRight = let
  hs = hashSymbol_withSlash <$> ["transRight","trr"]
  in HashKeyword
     { hashKeyword_name = "right transitive search"
     , hashKeyword_symbols = hs
     , hashKeyword_help = trans_blurb }

var :: HashKeyword
var = let
  hs = hashSymbol_withSlash <$> [ "var", "v" ]
  in HashKeyword
     { hashKeyword_name = "variable (pending)"
     , hashKeyword_symbols = hs
     , hashKeyword_help = "This isn't implemented yet. Its purpose is to permit the use of the Qseq meta-search language." }
