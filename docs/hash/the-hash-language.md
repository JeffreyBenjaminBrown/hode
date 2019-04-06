# The Hash Language

(If you're hurrying to the part where you can actually use the app,
you could skip the "advanced queries" section.)


# What Hash is

Hash is a language for reading and writing a `Rslt`.
Even though a `Rslt` is more complex than a graph,
Hash is simpler than common graph-writing languages (such as Turtle),
and *way* simpler than other graph query languages (such as Sparql or Gremlin).


# Writing to a Rslt with Hash

If you've read about [the Rslt](docs/the-rslt.md), you know how to write `Expr`s.
To add them to a `Rslt` using the UI,
you'll only need one extra symbols: "/add" (or "/a").

For instance, `/add Kurt #played guitar` creates a "played" relationship between "Kurt" and "guitar".
If any of those things ("Kurt", or "guitar", or the "_ played _" relationship) didn't exist before,
they do now.

(Because I feel like writing out the word /add is too much work,
there are 2-character abbreviations available for every common keyword.
See the "abbreviations" file in the documentation.)


## Special characters, quotes and escape characters

Hash uses the following special characters: `# & | / \ ( )`.
Most of them are only used for querying,
as explained in the rest of this document.
However, when adding (not querying for) data in a `Rslt`,
one has to be aware of them.
To enter a `Phrase` involving any of those special characters,
just enclose it in parentheses.
Within those parentheses,
if you need to write a literal quotation mark or `\` character,
you can do so by "escaping" it,
that is, by putting a `\` in front of it.

For instance, 
`"I said, \"Hi!\" (It was easy.)"` 
is a valid `Phrase`.
The parentheses don't need escaping,
because they are inside quotation marks.
The inner quotation marks do need escaping, though.
(Otherwise the parser would think you were done writing the phrase
when it encountered the quotation mark to the left of `Hi`).


# Querying a RSLT with Hash

Querying is a little more complex than writing,
because we query for multiple expressions at once.
We still use the same language, Hash,
but we introduce a few "reserved words".
Each of them is preceded by the `/` symbol.

Every query starts with the symbol "/find" (or "/f").


## Basic queries
### Query for a Phrase by writing it

`/find bob` will display the `Expr` "bob", if it is present.
This is good for two things:
determining whether it's in the database,
and finding its address.


### Query for a Hash expression by, again, writing it

For instance,
`/find bob #flattered alice` will search for and return the `Expr`
`bob #flattered alice`,
if it is present.


### Query for anything using the wildcard `/_`

For instance, if (in the database) bob has flattered alice and chuck,
then the command `/find bob #flattered /_`
would return "bob #flattered alice" and "bob #flattered chuck".

If you only want it to return alice and chuck,
rather than the entire #flattered relationship,
you can use the keywords `/eval` and `/it`
(see the "advanced queries" section below).


### Query for an Addr with /addr (or /@), followed by a number

For instance, if "bob" is stored at `Addr` 1,
then `/f /@ 1 #flattered /_` will find every expression of the form "bob #flattered /_".


### Set operations: union (`|`), interseciton (`&`), and difference (`\`)

`(/eval I #like /it) & (/eval you #like /it)` will return everything that you and I both like.
`(/eval I #like /it) | (/eval you #like /it)` will return everything that at least one of us likes.
`&` is called the "intersection" operator,
and `|` the "union" operator.

`\` represents the "difference" operator.
`(/eval you #like /it) \ (/eval I #(cannot afford) /it)`
will list the things you like, minus the ones I cannot afford.


## Advanced queries

### Replace a superexpression with a subexpression using `/eval` and `/it`

If (in the database) bob has flattered both alice and chuck,
then the command `/find /eval bob #flattered /it` would return "alice" and "chuck".

That tells the interpreter
"I am looking for the thing in the superexpression marked /eval that occupies the position marked /it.
Rather than returning the /eval superexpression,
only return the /it subexpression."

The `/eval` keyword is necessary because otherwise the parser would not know which superexpression to replace witht the expression in the `/it` position.

For instance, consider the following two similar-looking queries:

```
/find  /eval /it #breathes CO2  #eats bugs
/find (/eval /it #breathes CO2) #eats bugs
```

The first query doesn't make sense.
It will look for arity-3 expressions of the form
"/it #breathes CO2 #eats bugs",
and return the `/it`.

The second query is reasonable.
It will first find every expression X for which "X breathes CO2",
and then find the subset of those X-values for which "X eats bugs",
and return a set of expressions of the form "X eats bugs".
For instance, it might return "the venus flytrap #eats bugs".

(You can actually include more than one `/it` in an `/eval` statement.
For instance, "/eval /it #married /it" would return every married person,
regardless of whether they are listed first or second in the marriage relationship.)


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

Consider the command `/find /roles (1 a) (2 a # b)`.
This returns all relationships for which the first member is the word "a"
and the second is the relationship "a # b".

(There's also a way to specify the template,
but in that case it's not clear why you wouldn't use a hash expression instead of /map.)


### You might (?) need to preced a Hash expression with the /hash keyword

For instance,
`/find /hash bob #flattered alice` will search for and return
the `Expr` "bob #flattered alice",
if it is present.

I have tried to render the `/hash` keyword unnecessary,
but if you find something's not parsing that ought to,
it might help.
