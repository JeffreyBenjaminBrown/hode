# Why use Hash?

[Hash](the-hash-language.md) is a DSL for encoding natural language in a graph, inspired by [Markdown](https://daringfireball.net/projects/markdown/syntax). Hash works with ordinary natural language (Swahili, Chinese, anything), and introduces only one new symbol, the `#` symbol.

Reasons to use Hash include:

## Simplicity
The [language specification](the-hash-language.md) is only ten sentences long, and that's mostly examples. There is only one rule to learn.

## Natural order
Hash looks like natural language. The order in which a user enters an expression can correspond closely, often perfectly, to the language they already speak.

Consider the statement `nuclear power needs water for cooling`. It's a ternary (three-member) relationship between nuclear power, water, and cooling. To say that in other systems, you might have to do something awkward like `needs-for(nuclear power, water, cooling)`. By contrast, in Hash you would write `nuclear power #needs water #for cooling`.

## Relationships of arbitrary arity
In many systems a relationship must be binary -- that is, it must have exactly two members. For instance, edges in a graph connect exactly two nodes in the graph.

Sometimes binary relationships are expressive enough. The statements `Fred knows Mary` and `apples are food` fit easily into such a system. But what about `Romeo spoke poetry to Juliet`? That's a ternary `spoke-to`-relationship connecting Romeo, poetry, and Juliet.

Hash lets a user encode relationships of any arity, in the same uniform way.

## Compound relationships
Using Hash, relationships involving other relationships can be represented as easily as first-order relationships. `she #(smiles at) me ##when I #work hard`, for instance, represents a second-order `when`-relationship, which connects the two first-order relationships `she #(smiles at) me` and `I #work hard`.