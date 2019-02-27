There are four main branches to this project. Each branch is described in the [docs](docs) folder. They are -- and this is probably the order in which you should read about them -- these:


* The Rslt

The `Rslt` is the most general data structure I am aware of, a generalization of the graph. (It is what some programmers call a hypergraph, although mathematicians would beg to differ.)

A `Rslt`is a collection of `Expr`s (expressions), each of whcih is either a phrase or a relationship. The relationships can involve any number of members, and any relationship can itself belong to other relationships.

That's the idea, anyway. There are actually two more kinds of `Expr`; see the [docs](docs) folder for details.


* Hash

This is a language, close to ordinary natural language, for talking about a `Rslt`. It offers a concise representation both of individual `Expr`s (expressions, i.e. members) of a `Rslt` and for queries meant to retrieve subsets of an `Rslt`.


* The UI

It's pretty basic but it works.


* Qseq

This is a metalanguage for search. It lets you combine multiple searches in some underlying search language. `Qseq` hasn't made it's way into the UI yet, but it's implemented.
