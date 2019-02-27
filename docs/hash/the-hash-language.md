# The Hash Language
## Table of contents
* What the RSLT and the Hash language are
* Adding to a RSLT with Hash
  * Using * to create simple and compound relationships
    * A simple (level-1) binary relationship
    * A simple (level-1) ternary relationship
    * A compound (level-2) relationship
    * Unary relationships
    * Any number of * marks is valid
  * Parentheses help, too
  * Relationship templates can be in relationships
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


# What the RSLT and the Hash language are

Hash is a language for reading and writing a RSLT. A RSLT is [like a graph, but more expressive](/introduction/the_rslt,_why_and_how/it.pdf). Hash is simpler than other graph-writing languages (such as Turtle), and far simpler than other graph-reading languages (such as Sparql or Gremlin).


# Writing to a RSLT with Hash

Being a human fluent in some natural language, you already know about words and parentheses. To add data to a RSLT in Hash, you only need one more symbol, `#`.


## Using # to create simple and compound relationships
### A simple (level-1) binary relationship
`Bob #needs legal counsel` creates a binary `needs`-relationship between `Bob` and `legal counsel`. `Bob` and `legal counsel` could be called level-0 expressions. The `#` indicates that `needs` lies one level above them, forming a relationship that connects them. If `Bob` and `legal counsel` were not in the RSLT before, they are now; if they were, they are not duplicated.

The "relationship template" `_ needs _` is also added to the graph, if it is not already present. A relationship template is like a relationship, but it has no members. Relationships are built using relationship templates. (Note that when referring to a template, we don't write the `#` symbol. We only write that when *using* the relationship to connect things.)

`Bob` and `legal counsel` can be thought of as level-0 expressions. They contain no subexpressions. `Bob #needs legal counsel` is a level-1 expression. Loosely, `#` means "make it higher" or "make it connect the others".


### A simple (level-1) ternary relationship
`Bob #needs tech support #for teleconferencing` represents a ternary `needs-for`-relationship, between `Bob`, `tech support` and `teleconferencing`.

A binary relationship, such as "Van #studies set theory" has one "joint" between two "members". A ternary relationship, such as "Van #studies set theory #on sundays`, has two joints and three members. Etc.


### A compound (level-2) relationship
In a level-2 relationship, at least one member is itself a level-1 relationship. An example is `Bob #started working ##on january 1 2017`. That creates an `on`-relationship between the level-1 relationship `Bob #goes skiing` and the level-0 expression `january`.


### Unary relationships
A "relationship" does not have to have multiple members; it can be "unary". `maybe` and `not` are two important unary relationships. For instance, `##maybe we #need debt restructuring` creates a `maybe`-relationship with only one member, `we #need debt restructuring`.


### Any number of # marks is valid
`reasonable people #like watermelon ##when the weather #is hot ###because watermelon #is cold`.


## Parentheses help, too
The preceding statement regarding watermelon could be rewritten as `(reasonable people #like watermelon ##when the weather #is hot) #because (watermelon #is cold)`.

Parentheses can be used for joints as well -- as in, for instance, `gold #(produced by) a goose`.


## Relationship templates can be in relationships
As described earlier, when you add `a #needs b` to the graph, the effect is to add (if they don't already exist) `a`, `b`, the relationship `a #needs b`, and the relationship template `_ needs _`. That template can itself be in a relationship.

As an example, adding `(_ needs _) #(is equivalent to) (_ requires _)` would create (if they don't already exist) the "needs" template, the "requires" template, and the "is equivalent to" template, and would use the "is equivalent to" template to relate the other two.


# Querying a RSLT with Hash

Querying is a little more complex than writing, because we query for multiple expressions at once. We still use the same language, Hash, but we introduce a few "reserved words". Each of them is preceded by the `/` symbol.


## Basic queries
### Query for one thing by writing it
A query does not have to involve any reserved words. The query `hummingbirds #are amazing` will return the expression `hummingbirds #are amazing` if it is in the graph. If it is not, it will return nothing.


### Query for everything using /all
The simplest query for multiple things is `/all`. That will return every node in the graph.


### Query with wildcards using /any
Suppose the RSLT contained
```
I #must own a suit #by Friday
I #must locate Chicago #by Friday
I #must attend Eric and Stacy's wedding #on Saturday
```

`I #must /any #by Friday` would then return the first two expressions. `/any` is a "wildcard": it matches any expression.


### Query for subexpressions using /it
Using the same data, if we instead queried for `I #must /it #by Friday`, we would get `locate Chicago` and `own a suit`. The `/it` symbol causes the query to return the subexpression that appears where `/it` appears in the query, rather than the entire expression that matched the query.


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

To do that, use the `/eval` keyword. `Bran #likes (/eval /it #is psychopathic)` will return what we want.


### Search recursively using /branch, /from and /to

Suppose we had this data:
```
the spaceship #needs the fuel pod
the spaceship #needs GPS connectivity
the fuel pod #needs petroleum
```

We might want to find everything the spaceship needs. Needs is a transitive relationship: if a needs b and b needs c, then a needs c. Thus the query `the spaceship #needs /it` is insufficient: it will not recognize that the spaceship needs petroleum. 

Here is how to specify a recursive search, starting from `the spaceship`, using the `_ needs _` relationship:

`/branch (/from #needs /to) the spaceship`

The keywords `/from` and `/to` indicate the direction to travel along the `needs` relationship.

Searches involving relationships with more than two members are specified the same way. For instance, 

`/branch (/from #needs /to #on Sunday) the spaceship`

would return everything that the spaceship needs on Sunday, and

`/branch (/from #needs /to #on /any) the spaceship`

would return everything that the spaceship needs at any time.


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