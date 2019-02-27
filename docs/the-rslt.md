# What is a RSLT

A `Rslt` is a collection of `Expr`s. The `Expr` data type is defined like this:

```
data Expr =
    Word String                 -- word or phrase
  | Rel  [Expr] Expr            -- relationship
  | Tplt [Expr]                 -- template for relationships
  | Par [(String, Expr)] String -- paragraph
  | Addr Addr                   -- address
```


## The foundation: `Word`, `Rel` and `Tplt`

The `Word` is the basic building block of a `Rslt`. "dogs", "four score and seven years ago", and the empty string are all technically valid `Word`s.

A `Rel` is a relationship between some sub-`Expr`s. An example would be "frogs #eat bugs", which is an "eats" relationship between "frogs" and "bugs". Another would be "Bill #gave flowers #to Mary", which is a 3-member "gave-to" relationship. Yet another would be "#maybe not", which is a "maybe" relationship with the single member "not".

The first, `[Expr]` field of a `Rel` indicates the `Rel`'s members. The second, `Expr` field indicates its `Tplt` (template). The template is what lets us distinguish the relationship "Bill #likes Mary" from the relationship "Bill #confuses Mary".

A `Tplt` contains a list of `Expr` values, which we might call "joints". In the relationshiop "Bill #gave flowers #to Mary", the `Tplt` has two joints: "gave" and "flowers". A joint can involve multiple words, in which case it helps to use parentheses, as in, "Bill #(gave or intended to give) flowers #to Mary".

A convenient way to depict a `Tplt` is to intersperse an underscore where a member would go. For instance, we could refer to the "likes" `Tplt` as "_ likes _", and the "maybe" `Tplt` as "maybe _".


## Compound `Rel`s

"Mary #knows (Bill #likes Sue)" is compound: It is a binary "knows" relationship between "Mary" (which is a word, not a relationship) and the binary relationship "Bill #likes Sue". We say the "knows" relationship is "higher" than the "likes" relationship.

We can vary the number of hash symbols attached to a joint to indicate how high a relationship is. It makes things easier to read. For instance, the previous relationship could be written "Mary ##knows Bill #likes Sue". (When you're writing data into an `Rslt` you can use either style, mixed together in the same expression however you want. When you read `Expr`s from an `Rslt` they are (so far only) displayed without parentheses.)


## Par

A `Par` is just like a `Rel`, except its `Tplt` is not reified. `Par`s are good for writing long passages of text that you don't intend to search verbatim for. For instance, I might record the following `Par`:

```
The other day I was at the (/word beach) and I saw a (/word shark). That totally sucked.
```

If I did that, I could search for `Expr`s involving words "beach" or "shark", and the `Par` would be among the search results. But if I searched for "day" or "sucked" or "was at the" it would not be.


## Addr

This is a handy way for referring to an `Expr` rather than writing it all out. When an `Expr` appears in the UI, one view option is (or will soon be) to have its address appear to the left of the screen. That address can be referred to when creating new `Expr`s, or when searching for `Expr`s.

For instance, suppose "giant volcano turtles #lay eggs" is stored at `Addr` 13. In that case, we could create the expression "giant volcano turtles #lay eggs ##during spring" by writing the shorter expression "/addr 13 #during spring".


## The acronym

`Rslt` stands for `reflexive set of labeled tuples`. The `Tplt`s are the labels. Every `Expr` is (expressible as) a tuple. The word `reflexive` is supposed to indicate that relationships can be members of each other.

It's kind of stale. I would use "hypergraph", but mathematicians invented the hypergraph, so when you google "hypergraph" you get what they've written about, and it's something very different.
