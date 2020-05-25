You don't need to memorize all these commands.
They're easy to find in the in-app help.
Read this just to get a sense of what's possible.

# What Hash is

Hash is a language for reading and writing a `Rslt`.
Even though a `Rslt` is more complex than a graph,
Hash is simpler than common graph-writing languages (e.g. Turtle),
and *way*,
*way* simpler than other graph query languages (e.g. Sparql or Gremlin).

# Writing to a Rslt with Hash

If you've read about [the Rslt](docs/the-rslt.md),
you know how to define `Expr`s.
To add them to a `Rslt` using the UI,
you'll only need one extra symbols: `/add` (or `/a`).

For instance, `/add Kurt #played guitar` creates a "played" relationship between "Kurt" and "guitar".
If any of those things ("Kurt", or "guitar", or the "_ played _" relationship) didn't exist before,
they do now.

(Because I feel like writing out the word `/add` is too much work,
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

Every query starts with the symbol `/find` (or `/f`).

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
would return `bob #flattered alice` and `bob #flattered chuck`.

If you only want it to return "alice" and "chuck",
rather than the entire "flattered" relationship,
you can use the keywords `/eval` and `/it`
(see the "advanced queries" section below).

### Query for an `Addr` with `/addr` (or `/@`), followed by a number

For instance, if "bob" is stored at `Addr` 1,
then `/f /@ 1 #flattered /_` will find every expression of the form `bob #flattered /_`.

### Query for multiple `Addrs` with `/addrs` (or `/@s`), followed by numbers and (dash-separated) ranges of numbers

For instance, the command "/f /@s 1 10-12 42" would display the five `Expr`s located at the following `Addr` values: 1, 10, 11, 12 and 42.

### Set operations: union (`|`), interseciton (`&`), and difference (`\`)

`(/eval I #like /it) & (/eval you #like /it)` will return everything that you and I both like.
`(/eval I #like /it) | (/eval you #like /it)` will return everything that at least one of us likes.
`&` is called the `intersection` operator,
and `|` the `union` operator.

`\` represents the `difference` operator.
`(/eval you #like /it) \ (/eval I #(cannot afford) /it)`
will list the things you like, minus the ones I cannot afford.

## Advanced queries

### Query for superexpressions using `/member` (or `/m` and `/involves` (or `/i`)

If you'd like to find every relationship with "salsa"
as a top-level member,
you can write "/member salsa".

Equivalently, you could write "/involves-1 salsa".

If you'd like to find anything for which "salsa"
is in one of the top two levels,
you can write "/involves-2 salsa".

You can write "/involves-k" for any positive value of "k".
If you ask for a big value, the search might be slow.

### Replace a superexpression with a subexpression using `/eval` and `/it` (or `/it=`)

(Note: This is about replacement within search results. To replace one `Expr` with another in an `Rslt`, see the section on the `/replace` keyword in [the ui documentation](docs/ui.md).)

If (in the database) bob has flattered both alice and chuck,
then the command `/find /eval bob #flattered /it` would return "alice" and "chuck".

That tells the interpreter
"I am looking for the thing in the superexpression marked `/eval` that occupies the position marked `/it`.
Rather than returning the entire relationship `bob #flattered /it`,
the search will only return the `/it` subexpression."

The `/eval` keyword is necessary because otherwise the parser would not know which superexpression to replace with the expression in the `/it` position.

For instance, consider the following two similar-looking queries:

```
/find  /eval /it #breathes CO2  #eats bugs
/find (/eval /it #breathes CO2) #eats bugs
```

The first query is nonsense.
It will look for arity-3 expressions of the form
`/it #breathes CO2 #eats bugs`,
and try to return the `/it` part.
But it won't find anything, because the relationship "_ breathes _ eats _"
makes no sense.

The second query is reasonable.
It will first find every X for which `X breathes CO2`,
and then find the subset of those X-values for which `X eats bugs`,
and return a set of expressions of the form `X eats bugs`.
For instance, it might return `the venus flytrap #eats bugs`.

You can actually include more than one `/it` in an `/eval` statement.
For instance, `/eval /it #married /it` would return every married person,
regardless of whether they are listed first or second in the marriage relationship.

#### Using `/it=` to restrict the possible targets in an `/eval` query

You might want to restrict the set of possibilities considered for the "/it" variable(s) in an "/eval" expression. For instance, if you want to know who among Jane and Jim is coming to your wedding, you could ask:

`/f /eval (/it= Jane | Jim) #is invited to my wedding`

And if you didn't want to have to list all the possibilities explicitly,
you could use a nested "/eval' statement to, say,
ask which of your classmates is invited:

`/f /eval (/it= (/eval /it #is a classmate of mine)) #is coming to my wedding`

#### PITFALL: `/it=` cannot be followed by an alphanumeric character

`/it=` is a keyword, just like `/it` or `/eval`, which means it must be followed by a non-alphanumeric character (typically a space or a left parenthesis).b Just like writing `/italy` or `/evaleye` would confuse the parser, so too will writing `/it=x` confuse the parser.

### (Reflexive) transitive search

Suppose your graph has the two relationships "0 #< 1" and "1 #< 2",
and you'd like to find everything that is greater than or equal to 0.
You can find out by running the query `/f /tr 0 #< /_`.
Similarly, you can find everything less than or equal to 2 by running
`/f /tr /_ #< 2`.

Hash treats any relationship used in a transitive search as reflexive.
Thus even though we called the relationship "#<", it is effectively "<=".

#### PITFALL: Hode believes you

Hode does not know which of your relationships "should" be transitive;
it only treats a relationship as such when specifically asked to do so.

#### PITFALL: Cycles
If you're not sure whether a relationship forms cycles --
for instance, not just "0 #< 1" and "1 #< 2", but also "2 #< 0" --
then save your work before searching it transitively.

That's because Hode will crash if it encounters a cycle.
It won't respond to keypresses, and you'll need to kill it from outside.

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

### You might (?) need to precede a Hash expression with the /hash keyword

For instance,
`/find /hash bob #flattered alice` will search for and return
the `Expr` "bob #flattered alice",
if it is present.

I have tried to render the `/hash` keyword unnecessary,
but if you find something's not parsing that ought to,
it might help.
