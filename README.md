See below for how to install.


# What this is

There are four main branches to this project.
Each branch is described in the [docs](docs) folder.
They are -- and this is probably the order in which you should read about them -- these:


## The Rslt

The `Rslt` is the most general data structure I am aware of.
It is a generalization of the graph. (
It is isomorphic to what some programmers call a "hypergraph" -- but mathematicians claimed that term first, and in math it means something completely different.)

A `Rslt`is a collection of `Expr`s (expressions), each of whcih is either a phrase or a relationship.
The relationships can involve any number of members, and any relationship can itself belong to other relationships.

That's the idea, anyway.
There are actually two more kinds of `Expr`; see the [docs](docs) folder for details.


## Hash

`Hash` is a language, close to ordinary natural language, for talking about a `Rslt`.
`Hash` offers a concise representation both of individual `Expr`s (expressions, i.e. members) of a `Rslt` and for queries meant to retrieve subsets of a `Rslt`.


## The UI

It's the most convenient way to do the things you'll most often want to do -- load data, save data, insert data, search for data, view data.


## Qseq

(`Qseq` hasn't made it's way into the UI yet, but it's implemented.)

This is a metalanguage for search.
It lets you combine multiple searches in some underlying search language.


# How to install

You'll need `Stack` (the Haskell toolkit) installed.
You'll also need `xsel`, and proabbly a few things I can't remember (`libxrandr-dev`, `lbxss-dev` and `libx11-dev`, maybe?).

Clone the repo and then run `stack ghci` from somewhere inside it.
Then run "ui" to start the UI.
