# The Hash Language

(If you're hurrying to the part where you can actually use the app, you could skip the "advanced queries" section.)


# What Hash is

Hash is a language for reading and writing a `Rslt`.
Even though a `Rslt` is more complex than a graph, Hash is simpler than common graph-writing languages (such as Turtle), and *way* simpler than other graph query languages (such as Sparql or Gremlin).


# Writing to a Rslt with Hash

If you've read about [the Rslt](docs/the-rslt.md), you know how to write `Expr`s.
To add them to a `Rslt` using the UI, you'll just need two extra symbols: "/add" and "/hash".
"/add" indicates that you're inserting a new expression.
"/hash" indicates that what follows is a Hash expression.

So, for instance, `/add /hash Kurt #played guitar` creates a "played" relationship between "Kurt" and "guitar".
If any of those things didn't exist before, they do now -- including the template "_ played _".

(Should typing "/hash" repeatedly seems annoying, take heart!
There are 2-character abbreviations available for every common keyword.
See the "abbreviations" file in the documentation.)


## Special characters, quotes and escape characters

Hash uses the following special characters: `# & | / \ ( )`.
Most of them are only used for querying, as explained in the rest of this document.
However, when adding (not querying for) data in a `Rslt`, one has to be aware of them.
To enter a `Phrase` involving any of those special characters, just enclose it in parentheses.
Within those parentheses, if you need to write a literal quotation mark or \ character, you can do so by "escaping" it, that is, by putting a "\\" in front of it.

For instance, `"I said, \"Hi!\" (It was easy.)"` is a valid `Phrase`.
The parentheses don't need escaping, because they are inside quotation marks.
The inner quotation marks do need escaping, though.
(Otherwise the parser would think you were done writing the phrase when it encountered the quotation mark to the left of `Hi`).


# Querying a RSLT with Hash

Querying is a little more complex than writing, because we query for multiple expressions at once.
We still use the same language, Hash, but we introduce a few "reserved words".
Each of them is preceded by the `/` symbol.

Every query starts with the symbol "/find" (or "/f").


## Basic queries
### Query for a Phrase by writing it

`/find bob` will display the `Expr` "bob", if it is present.
This is good for two things: determining whether it's in the database, and finding its address.


### Query for a Hash expression by preceding it with /hash (or /h)

For instance, `/find /hash bob #flattered alice` will search for and return the `Expr` "bob #flattered alice", if it is present.


### Query for anything using the wildcard `/_`

For instance, if (in the database) bob has flattered alice and chuck, then the command `/find /hash bob #flattered /_` would return "bob #flattered alice" and "bob #flattered chuck".

If you only wanted it to return alice and chuck, rather than the entire #flattered relationship, you can use the keywords `/eval` and `/it`.


### Query for an Addr with /addr (or /@), followed by a number

For instance, if "bob" is stored at `Addr` 1, then `/f /h /@ 1 #flattered _` will find every expression of the form "bob #flattered _".


### Boolean operations: union (|), interseciton (&), and difference (\)
`(I #like /it) & (you #like /it)` will return everything that you and I both like. `(I #like /it) | (you #like /it)` will return everything that either of us like. `&` is called the "intersection" operator,  and `|` the "union" operator.

`(you #like /it) \ (I #(cannot afford) /it)` will list everything you like except the things I cannot afford.


## Advanced queries

### Replace a superexpression with a subexpression using `/eval` and `/it`

If (in the database) bob has flattered alice and chuck, then the command `/find /hash /eval bob #flattered /it` would return "alice" and "chuck".

That tells the interpreter "I am looking for the thing in the superexpression marked /eval that occupies the position marked /it.
Rather than returning the /eval superexpression, only return the /it subexpression."

The `/eval` keyword is necessary because otherwise the parser would not know which superexpression to replace witht the expression in the `/it` position.

For instance, consider the following two similar-looking queries:

```
/find /hash (/eval  /it #breathes CO2) ##eats bugs
/find /hash (/eval (/it #breathes CO2 ##eats bugs))
```

Suppose the venus flytrap is the only thing in our database that breathes CO2, and further suppose that it eats bugs.
The first search will return "the venus flytrap #eats bugs":
it has replaced the superexpression "/it #breathes CO2" with the member in the "/it" position.

The second query doesn't make sense:
It will look for expressions of the form
"(<something> #breathes CO2) #eats bugs",
and then return the `<something>`.
But "<something> #breathes CO2" is a sentence,
and sentences do not eat bugs.


### Count &, | and \ symbols like # symbols
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

Consider the command `/find /roles (1 a) (2 /hash a # b)`.
This returns all relationships for which the first member is the word "a" and the second is the relationship "a # b".
(There's even a way to specify the template,
but in that case it's not clear why you wouldn't use a /hash expression instead of /map.)
