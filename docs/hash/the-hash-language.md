# The Hash Language

(If you're hurrying to the part where you can actually use the app, I would skip the "advanced queries" section.)

## Table of contents
* What Hash is
* Writing to a Rslt with Hash
* Querying a RSLT with Hash
  * Basic queries
    * Query for one thing by writing it
    * Query for everything using /all
    * Query with wildcards using /any
    * Boolean operations: union (|), interseciton (&), and difference (\)
  * Advanced queries
    * Early evaluation for sub-queries: Use /eval
    * Search recursively using /branch, /from and /to
    * Count &, | and \ symbols like * symbols


# What Hash is

Hash is a language for reading and writing a `Rslt`. Even though a `Rslt` is more complex than a graph, Hash is simpler than common graph-writing languages (such as Turtle), and far simpler than other graph query languages (such as Sparql or Gremlin).


# Writing to a Rslt with Hash

If you've read about [the Rslt](docs/the-rslt.md), you know how to write `Expr`s. To add them to the `Rslt` using the UI, you'll just need to prefix two symbols: "/add" and /hash". The first one indicates that you're inserting a new expression. The second indicates that what follows is a Hash expression. So, for instance, `/add /hash Kurt #played guitar` creates a "played" relationship between "Kurt" and "guitar". If any of those things didn't exist before, they do now -- including the template "_ played _".


# Querying a RSLT with Hash

Querying is a little more complex than writing, because we query for multiple expressions at once. We still use the same language, Hash, but we introduce a few "reserved words". Each of them is preceded by the `/` symbol.

Every query starts with the symbol "/find".


## Basic queries
### Query for a Word by writing it

For instance, `/find bob` will display the `Expr` "bob", if it is present. This is good for two things: determining whether it's in the database, and finding its address.


### Query for a Hash expression by preceding it with the word /hash

For instance, `/find /hash bob #processed alice` will search for and return the `Expr` "bob #processed alice".


### Query for everything using _

For instance, if bob has processed alice and chuck, then the command `/find /hash bob #processed _` would return "bob #processed alice" and "bob #processed chuck".

If you only wanted it to return alice and chuck, rather than the entire #processed relationship, you could use the keyword /it.


### Query for subexpressions using /it

For instance, if bob has processed alice and chuck, then the command `/find /hash bob #processed /it` would return "alice" and "chuck".

(PITFALL: This will change soon, but at the moment, in order for an expression involving the /it keyword to be evaluated properly, the keyword /eval must be used at the top. For instance, the above example needs to be written as `/find /eval /hash bob #processed /it`.)


### Query for an Addr with /addr, followed by a number

For instance, if "bob" is stored at `Addr` 1, then `/find /hash /addr 1 #processed _` will find every expression of the form "bob #processed _".


### Boolean operations: union (|), interseciton (&), and difference (\)
`(I #like /it) & (you #like /it)` will return everything that you and I both like. `(I #like /it) | (you #like /it)` will return everything that either of us like. `&` is called the "intersection" operator,  and `|` the "union" operator.

`(you #like /it) \ (I #(cannot afford) /it)` will list everything you like except the things I cannot afford.


## Advanced queries

### Early evaluation for sub-queries: Use /eval

Consider a RSLT with the following data:
```
Bran #likes the dog
Bran #likes the cat
the cat #is psychopathic
```
We might try the query `Bran #likes (/it #is psychopathic)`, as a way to find all the psychopathic things that Bran likes. That query would not work, however, because it specifies a search for a #likes relationship in which the second member is an #is relationship. We want the expression `/it #is psychopathic` to be evaluated in its entirety, not as a subexpression.

To do that, use the `/eval` keyword. `Bran #likes (/eval /it #is psychopathic)` will return "Bran #likes the cat".


### Count &, | and \ symbols like # symbols
The set operators `&`, `|` and `\` can be repeated, just like the `#` symbol, to decrease their precedence (making them "bind later"). For instance, rather than 
```
(/it #helps Democrats) & (/it #helps Republicans)
```

you could write 
```
/it #helps Democrats && /it #helps Republicans
```

It saves three keystrokes, and is arguably more readable.


### Query for "Hash maps" using /roles or /map

Consider the command `/find /roles (1 a) (2 /hash a # b)`. This returns all relationships for which the first member is the word "a" and the second is the relationship "a # b". (There's even a way to specify the template, but in that case it's not clear why you wouldn't use a /hash expression instead of /map.)
