You don't need to memorize ethese commands,
because they're easy to find in the in-app help.
Read this just to get a sense of what's possible.

# What Hash is

Hash is a language for writing and searching a `Rslt`.
Even though a `Rslt` is more complex than a graph,
Hash is simpler than common graph-writing languages (e.g. Turtle),
and *way*,
*way* simpler than other graph query languages (e.g. Sparql or Gremlin).

# Writing to a Rslt with Hash

If you've read about [the Rslt](docs/the-rslt.md),
you know how to define `Expr`s.
To add them to a `Rslt` using the UI,
you'll only need one extra symbol: `/add` (or `/a`).

(The `/add` symbol is not actually part of the Hash language;
it's part of the [UI language](../ui.md),
which is even simpler than Hash.)

For instance, `/add Kurt #played guitar` creates a "played" relationship between "Kurt" and "guitar".
If any of those things ("Kurt", or "guitar", or the "_ played _" relationship) didn't exist before,
they do now.

(`/add` can also be written more briefly, as `/a`.
Check the in-app help to find all the abbrevations available for any keyword.)

## Special characters, quotes and escape characters

(If your phrases are all alphanumeric you don't need to know any of this.)

Hash uses the following special characters: `# / ( ) \"`.
The `#` symbol, as we have seen,
is used to join expressions into relationships.
Parentheses are used to group Hash expressions --
`a # (b # c)` means something different from `(a # b) # c`.

The `/` symbol identifies keywords.
Every keyword except `#` must be prefixed by `/`.
If a word starts with `/` and is not a keyword,
Hode will throw an error.

To use any of the special symbols in a phrase,
just enclose the phrase in quotation marks.
That's what is special about quotation marks.

To make a quotation mark part of a phrase,
precede the quotation mark with the ordinary escape character '\',
and wrap the phrase in quotation marks.
For instance, if I want to add the relationship
`the Declaration of Independence #says "all men are created equal"`,
I would write this:
`/add the Declaration of Independence #says "\"all men are created equal\""`,

# Querying a RSLT with Hash

Querying is a little more complex than writing,
because a query can specify multiple expressions at once.
We still use the same language, Hash,
but we introduce a few keywords.

Every search starts with the symbol `/find` (or `/f`).
This, again, is not part of the Hash language,
but rather the dead-simple [UI language](../ui.md).

## Basic queries

### Query for a `Phrase` by writing it

`/find bob` will display the `Expr` "bob", if it is present.
This is good for two things:
determining whether it's in the database,
and finding its address.

### Query for a `Rel` expression by writing it

For instance,
`/find bob #flattered alice` will search for and return the `Rel`
`bob #flattered alice`,
if it is present.

### Query for anything using the wildcard `/_`

The `/_` symbol is a "wildcard": it represents anything at all. 
It is meaningless by itself, but useful as a sub-expression.

For instance, `Bob #likes /_` will match `Bob #likes orangutans` 
and `Bob #likes (diving #for doughnuts)` 
and any other relationship of the form `Bob #likes _`."

#### Sidenote: Why `/_` is meaningless by itself

Hode could have been written such that if you asked for `/_`, 
it would return everything in your graph. 
But you probably wouldn't want that, and it might crash your computer.

### Query for `Addr`s with `/addr` (or `/@`)

The /@ symbol precedes a specification of expressions via their addresses.
For instance, `/@ 1 3-5 8` represents every expression whose address is either 1, 3, 4, 5 or 8.
The /@ symbol can be followed by any number of integers (like `3`) and integer ranges (like `3-5`).
They don't have to be in order.

### Set operations: union (`/|`), interseciton (`/&`), and difference (`/\`)

Set operations are ways to combine sets. Given two sets A and B, we can consider their "intersection" (things in both), or their "union" (things in either), or their "difference" (things in one but not the other).

For instance:
`(/eval I #like /it) /& (/eval you #like /it)` gives the intersection:
it will return everything that you and I both like.
`(/eval I #like /it) /| (/eval you #like /it)` gives the union:
it will return everything that you or I like
-- that is, everything you like, and everything that I like.
`(/eval you #like /it) /\ (/eval I #like /it)` gives the difference:
it will return the things you like, minus the things that I like.

These operator can be chained: `a /& b /& c /& ...`
will find all expressions that match `a` and `b` and `c` ...

If you don't like using parentheses to control the order in which binary operators operate,
you can avoid them.
The set operatoions obey the same precedence rules as #.
For instance, `a /| b /&& c # d` means the same thing as
`(a /| b) /& (c # d)`:
Since `/&&` has two characters (the leading slash doesn't count),
and the others have only one, `/&&` binds after them.

### Query for a template with `/tplt` (or `/template` or `/t`)

Usually you'll query for phrases and relationships.
Every now and then, though, you might want to look for a specific template --
for instance, when using the `/map` (or `/roles`) keyword (described later).

For instance,
the query `/tplt /_ is /_` represents the binary `is` template --
the one used in relationships like 'swimming #is delicious'.
Each spot for a member in the relationship is marked using the `/_` wildcard.
The joints between those members can include multiple wordss:
`/tplt /_ is kind of /_`, for instance,
is the template used by the relationship `suddoku #(is kind of) fun`.

## Advanced queries

### Query for superexpressions using `/member` (or `/m`) and `/involves` (or `/i`)

The /member and /involves keywords are similar.
Both let you find the set of expressions containing some sub-expression,
without specifying precisely where the sub-expression should be.

For instance, to indicate every relationship with
`salsa` as a top-level member, you can write `/member salsa`.
This will find `salsa #has tomatoes` and `Jenny #hates salsa`
and `I #buy salsa #from Trader Joe's`.
It will not return `salsa`, because that's not a relationship.

`/member` only searches for top-level members.
Therefore, `/member salsa` will not return `Jenny #is (allergic #to salsa)`,
because salsa is not a top-level member of that relationship --
it is a level-2 member.

If you want to include more than top-level members, you can --
that's what `/involves` is for.
Whereas `/member` only allows you to search for top-level members,
`/involves` lets you search the top level, or the top two levels,
or the top three, etc. Returning to our example,
if you'd like to find everything for which `salsa`
is in one of the top two levels, you can write `/involves-2 salsa`.

You can write `/involves-k` for any positive value of `k`.
If you ask for a big value, the search might be slow.


### Return a subexpression with `/eval` and `/it` (or `/it=`)

The symbols `/eval`, `/it` and `/it=` 
are used to extract subexpressions from superexpressions.

We have already seen that if (in the database)
bob has flattered both alice and chuck,
then `/find bob #flattered /it` would return both `bob #flattered alice`
and `bob #flattered chuck`.
That is, it returns two `_ flattered _` relationships.

What if we don't want those relationships,
but instead just their right-hand members `alice` and `chuck`?
That's what `/eval` is for.
`/find /eval bob #flattered /it` would return `alice` and `chuck`.

Generally, the command tells the interpreter
"I am looking for the thing in the superexpression marked `/eval`
that occupies the position marked `/it`."

`/eval` expressions do not have to be top-level:
the results of `/eval` can be referred to by an outer expression.
For instance, consider the following two similar-looking queries:

```
/find (/eval /it #breathes through its skin) #eats bugs
/find  /eval /it #breathes through its skin  #eats bugs
```

The first query is reasonable.
It will first find every X for which `X #breathes through its skin`,
and then return every `X eats bugs` relationship involving one of those Xs.

The second query is nonsense.
It will look for arity-3 expressions of the form
`/it #breathes through its skin #eats bugs`,
and try to return the `/it` part.
But it won't find anything,
because the template "_ breathes _ eats _" makes no sense.

You can actually include more than one `/it` in an `/eval` statement.
For instance, `/eval /it #married /it` would return every married person,
regardless of whether they are listed first or second in the marriage relationship.

#### Using `/it=` to restrict the possible targets in an `/eval` query

You might want to restrict the set of possibilities considered for the "/it" variable(s) in an "/eval" expression. For instance, if you want to know who among Jane and Jim is coming to your wedding, you could ask:

`/f /eval (/it= Jane | Jim) #is invited to my wedding`

This way, if Jim is invited and Jane is not, 
you'll just see Jim in the results,
rather than Jim and everyone else who is invited.
(Note that there is another way to get the same result:
you could search for
`(Jane /| Jim) /& (/eval /it #is invited to my wedding)`.)

You can nest `/eval` statements.
For instance, if Hode were to evaluate the following query:

`/f /eval (/it= (/eval /it
                       #is a classmate of mine))
           #is coming to my wedding`

it would first (in the inner `/eval`) find every classmate of yours,
and then (in the outer `eval`) find which of them is coming to your wedding.
It would return a list of people, not `#is` relationships.
(Hode treats all whitespace as a single space; 
I only used newlines and big spaces above to make the query easier to read.)

#### PITFALL: `/it=` cannot be followed by an alphanumeric character

`/it=` is a keyword, just like `/it` or `/eval`,
which means it must be followed by a non-alphanumeric character
(typically a space or a left parenthesis).
Just as writing `/italy` or `/evalentine` would confuse the parser,
so too will writing `/it=x` confuse the parser.

### (Reflexive) transitive search

If `a > b` and `b > c`, you might want Hode to infer that `a > c`.
That's called transitivity.
See [the documentation on transitivity](docs/transitivity.md)
for details on how to create, search, and order the displayed results
based on transitivity.

### Count `&`, `|` and `\` symbols like `#` symbols

The set operators `&`, `|` and `\` can be repeated,
just like the `#` symbol,
to decrease their precedence (making them "bind later").
For instance, rather than
```
(/eval /it #helps Democrats) & (/eval /it #helps Republicans)
```

you could write
```
/eval /it #helps Democrats && /eval /it #helps Republicans
```

It saves three keystrokes, and is arguably more readable.

### Query for "Hash maps" using /roles or /map

Consider the command `/find /roles (1 a) (2 b)`.
This returns all relationships for which the first member is the word "a"
and the second is "b".
Notice that unlike, say, the expression `a #eats b`,
the `/roles` idiom lets you leave the template unspecified.
It also lets you put other restrictions on the template,
specifying it somewhat but not completely.

In addition to the keywords `1`, `2`, etc. (any positive integer),
the keyword `t` specifies the template.
For instance, `/roles (t /t /_ is /_) (1 bill)`
is (pointlessly verbose but) equivalent to `bill #is /_`.

What follows each of the keywords `1`, `2`, ... and `t`
can be an arbitrary Hash expression.
For instance, the following identifies everything for which the template
is either the binary `is` template or the expression at `Addr 7`:
`/roles /t (/_ is /_) | (/@ 7)`.

### You might (?) need to precede a Hash expression with the /hash keyword

For instance,
`/find /hash bob #flattered alice` will search for and return
the `Expr` "bob #flattered alice",
if it is present.

I have tried to render the `/hash` keyword unnecessary,
but if you find something's not parsing that ought to,
it might help.
