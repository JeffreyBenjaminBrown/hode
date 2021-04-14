# What is a RSLT

A `Rslt` (recursive set of labeled tuples)
is a collection of `Expr`s (expressions).
Every `Expr` takes one of four forms.

## Building blocks: `Phrase`, `Rel` and `Tplt`

`Phrase`s can be any string of symbols
`dogs`, `four score and seven years ago`, `-~~ !*&%`
and the empty string are all valid `Phrase`s.

A `Rel` is a relationship between some sub-`Expr`s.
An example would be `frogs #eat bugs`,
which is an `eats` relationship between `frogs` and `bugs`.
Another would be `Bill #gave flowers #to Mary`,
which is a 3-member `gave-to` relationship.
Yet another would be `#maybe I am immortal`,
which is a `maybe` relationship with the single member `I am immortal`.

There are two pieces of data needed to define a `Rel`.
The first are its members --
for instance, `frogs` and `bugs` in the first example above.
The second piece of information needed, the `Tplt` (template),
defines what relationship holds between those members.
Two relationships can have the same members and still be distinct,
so long as their templates are different.
For instance, in the `Rel` `frogs #eat bugs`, the `Tplt` is `_ eat _`,
whereas in the `Rel` `frogs #(inhabit fewer places than) bugs`,
it is `_ inhabit fewer places than _`.

A `Tplt` contains a list of `Expr` values,
which we might call "separators".
In the relationshiop `Bill #gave flowers #to Mary`,
the `Tplt` has two separators: `gave` and `to`.
A separator can involve multiple words,
in which case it helps to use parentheses,
as in, `Bill #(handed or threw) flowers #to Mary`.

A convenient way to depict a `Tplt`
is to intersperse an underscore where a member would go.
For instance, we could refer to the `likes` `Tplt` as `_ likes _`,
and the `maybe` `Tplt` as `maybe _`.

Note that the term "separator" is not exactly accurate:
A separator in a `Tplt` in Hode usually separates two things,
but in some cases it only has one neighbor.
Examples include the `maybe` in `#maybe I'm amazed`,
and the `sometimes` in `everybody #needs somebody #sometimes`.

## Compound `Rel`s

`Mary #knows (Bill #likes Sue)` is compound:
It is a binary `knows` relationship between `Mary`
(which is a phrase, not a relationship)
and the binary relationship `Bill #likes Sue`.
We say the `knows` relationship is "higher" than the `likes` relationship.
(In terms of operator precedence, higher relationships "bind later".)

## Addr

The `Addr` (address) a handy way for referring to an `Expr`
rather than writing it all out.
For short expressions it doesn't matter much,
but for long expressions it can be very helpful.

When an `Expr` appears in the UI,
its address appears (if the user so chooses) at its left.
That address can be referred to when creating new `Expr`s,
or when searching for `Expr`s.
For instance,
suppose `giant volcano turtles #lay really hot eggs` is stored at `Addr` 13.
In that case, we could represent
`(giant volcano turtles #lay really hot eggs) #during spring`
with the shorter expression `/addr 13 #during spring`.

# Footnotes

## The definition in code

The definition in Haskell of a Rslt is roughly this:

```
data Expr =
    Phrase String    -- word or phrase. (can include spaces).
  | Rel  [Expr] Expr -- relationship
  | Tplt [Expr]      -- template for relationships
  | Addr Addr        -- the address at which some `Expr` can be found
```

That still omits a couple details;
the true definition can be found in
[Rslt.Types](../../hode/Hode/Rslt/Types.hs).
