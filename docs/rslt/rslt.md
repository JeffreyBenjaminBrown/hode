# What is a RSLT

A `Rslt` is a collection of `Expr`s.
Every `Expr` takes one of four forms:

```
data Expr =
    Phrase String    -- word or phrase
  | Rel  [Expr] Expr -- relationship
  | Tplt [Expr]      -- template for relationships
  | Addr Addr        -- the address at which some `Expr` can be found
```


## The foundation: `Phrase`, `Rel` and `Tplt`

The `Phrase` is the basic building block of a `Rslt`. "
dogs", "four score and seven years ago", and the empty string are all valid `Phrase`s.

A `Rel` is a relationship between some sub-`Expr`s.
An example would be "frogs #eat bugs", which is an "eats" relationship between "frogs" and "bugs".
Another would be "Bill #gave flowers #to Mary", which is a 3-member "gave-to" relationship.
Yet another would be "#maybe not", which is a "maybe" relationship with the single member "not".

The first, `[Expr]` field of a `Rel` indicates the `Rel`'s members.
The second, `Expr` field indicates its `Tplt` (template).
The template is what lets us distinguish the relationship "Bill #likes Mary" from the relationship "Bill #confuses Mary".

A `Tplt` contains a list of `Expr` values, which we might call "joints".
In the relationshiop "Bill #gave flowers #to Mary", the `Tplt` has two joints: "gave" and "flowers".
A joint can involve multiple words, in which case it helps to use parentheses, as in, "Bill #(gave or intended to give) flowers #to Mary".

A convenient way to depict a `Tplt` is to intersperse an underscore where a member would go.
For instance, we could refer to the "likes" `Tplt` as "_ likes _", and the "maybe" `Tplt` as "maybe _".


## Compound `Rel`s

"Mary #knows (Bill #likes Sue)" is compound: It is a binary "knows" relationship between "Mary" (which is a word, not a relationship) and the binary relationship "Bill #likes Sue".
We say the "knows" relationship is "higher" than the "likes" relationship.

We can vary the number of hash symbols attached to a joint to indicate how high a relationship is.
This is optional, but it makes things easier to read.
For instance, the previous relationship could be written "Mary ##knows Bill #likes Sue".
(When you're writing data into a `Rslt` you can use either style, mixed together in the same expression however you want.
When you read `Expr`s from a `Rslt` they are (so far only) displayed without parentheses.)


## Addr

This is a handy way for referring to an `Expr` rather than writing it all out.
When an `Expr` appears in the UI, its address appears to the left of the screen.
That address can be referred to when creating new `Expr`s, or when searching for `Expr`s.

For instance, suppose "giant volcano turtles #lay eggs" is stored at `Addr` 13.
In that case, we could represent "giant volcano turtles #lay eggs ##during spring" with the shorter expression "/addr 13 #during spring".
